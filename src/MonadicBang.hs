{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE PatternSynonyms #-}

module MonadicBang (plugin) where

import Control.Applicative
import Control.Monad.Trans.Maybe
import Control.Monad.State.Strict
import Control.Monad.Writer.CPS
import Data.Data
import Data.Functor
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import GHC
import GHC.Data.Bag
import GHC.Parser.Errors.Types
import GHC.Plugins hiding (Expr, empty)
import GHC.Types.Error
import GHC.Utils.Monad (concatMapM)
import Text.Printf

import Debug.Trace
import GHC.Hs.Dump

-- TODO: Write user manual as haddock comment

plugin :: Plugin
plugin = defaultPlugin
  { parsedResultAction = replaceBangs
  , pluginRecompile = purePlugin
  }

-- We don't care about which file things are from, because the entire AST comes
-- from the same module
data Loc = MkLoc {line :: Int, col :: Int}
         deriving (Eq, Ord, Show)

type Expr = HsExpr GhcPs
type LExpr = LHsExpr GhcPs

-- | Decrement column by one to get the location of a bang
addBang :: Loc -> Loc
addBang loc = loc{col = loc.col - 1}

-- | Used to extract the Loc of a located expression
pattern ExprLoc :: Loc -> Expr -> LExpr
pattern ExprLoc loc expr <- L (locA -> RealSrcSpan (spanToLoc -> loc) _) expr

spanToLoc :: RealSrcSpan -> Loc
spanToLoc = liftA2 MkLoc srcLocLine srcLocCol . realSrcSpanStart

replaceBangs :: [CommandLineOption] -> ModSummary -> ParsedResult -> Hsc ParsedResult
replaceBangs _ _ (ParsedResult (HsParsedModule mod' files) msgs) =
  -- trace (showSDocUnsafe $ showAstData BlankSrcSpan BlankEpAnnotations mod') $ -- XXX JB
  pure $ ParsedResult (HsParsedModule (let m' = (fillHoles fills mod') in trace (showSDocUnsafe $ ppr m') m') files) msgs{psErrors}
  where
    -- Take out the errors we care about, throw the rest back in
    (mkMessages -> psErrors, M.fromList . bagToList -> fills) =
      (partitionBagWith ?? msgs.psErrors.getMessages) \cases
        err | PsErrBangPatWithoutSpace lexpr@(ExprLoc (addBang -> loc) _) <- err.errMsgDiagnostic
            -> Right (loc, lexpr)
            | otherwise -> Left err

-- | Replace holes in an AST whenever an expression with the corresponding
-- source span can be found in the given list.
fillHoles :: Data a => Map Loc LExpr -> a -> a
fillHoles fillers ast = case runState (goNoDo ast) (MkFillState fillers) of
  (ast', state') | null state'.remainingErrors -> ast'
                 -- | otherwise -> error "Found extraneous bangs" -- TODO improve error msg (incl. bug report url)
                 --                                               -- Use PsUnknownMessage? (maybe not since this is a panic)
                 | otherwise -> trace "XXX JB WARNING" ast'
  where

-- TODO: embed the expression in existing or new do-notation
-- Approach: in tryFillHole, whenever we encounter let/where/do/etc.,
-- make a separate monadic traversal through the subtree with a writer monad,
-- adding the bindings we need to the monadic context so we can construct the
-- correct do block once the traversal is evaluated

    goNoDo :: forall a m . (MonadState FillState m, Data a) => a -> m a
    goNoDo e = maybe (gmapM goNoDo $ e) pure =<< runMaybeT (tryInsertDo e)

    -- surround the expression with a `do` if necessary
    tryInsertDo :: forall a m . (MonadState FillState m, Data a) => a -> MaybeT m a
    tryInsertDo expr = do
      Refl <- hoistMaybe (eqT @a @LExpr)
      (expr', stmts) <- runWriterT (goDo expr)
      if null stmts
        then pure expr'
        else let lastStmt = BodyStmt noExtField expr' noExtField noExtField
                 doStmts = (fromBindStmt <$> stmts) ++ [lastStmt]
             in pure . noLocA $
                  HsDo EpAnnNotUsed (DoExpr Nothing) (noLocA $ noLocA <$> doStmts)

    goDo :: forall a m . (MonadFill m, Data a) => a -> m a
    goDo e = maybe (gmapM goDo $ e) pure =<< runMaybeT (asum $ [tryLExpr, tryStmt, tryGRHSs] ?? e)

    tryStmt :: forall a m . (MonadFill m, Data a) => a -> MaybeT m a
    tryStmt e = do
      Refl <- hoistMaybe (eqT @a @(ExprStmt GhcPs))
      case e of
        RecStmt{recS_stmts} -> do
          recS_stmts' <- traverse addStmts recS_stmts
          pure e{recS_stmts = recS_stmts'}
        ParStmt xp stmtBlocks zipper bind -> do
          stmtsBlocks' <- traverse addParStmts stmtBlocks
          pure $ ParStmt xp stmtsBlocks' zipper bind
          where
            addParStmts :: ParStmtBlock GhcPs GhcPs -> MaybeT m (ParStmtBlock GhcPs GhcPs)
            addParStmts (ParStmtBlock xb stmts vars ret) = do
              stmts' <- addStmts stmts
              pure $ ParStmtBlock xb stmts' vars ret
        _ -> empty

    tryLExpr :: forall a m . (MonadFill m, Data a) => a -> MaybeT m a
    tryLExpr e = do
      Refl <- hoistMaybe (eqT @a @LExpr)
      ExprLoc loc expr <- pure e
      case expr of
        -- Replace holes resulting from `!`
        HsUnboundVar _ _ -> do
          lexpr' <- goDo =<< popError loc
          let name = mkVarName loc
          tell [name :<- lexpr']
          goDo $ e $> HsVar noExtField (noLocA name)
        -- If we encounter a `do`, use it instead of a `do` we inserted
        HsDo xd ctxt (L l stmts) -> (e $>) . HsDo xd ctxt . L l <$> addStmts stmts
        _ -> empty

    -- This lets us start new do-blocks in where blocks
    tryGRHSs :: forall a m . (MonadFill m, Data a) => a -> MaybeT m a
    tryGRHSs e = do
      Refl <- hoistMaybe (eqT @a @(GRHSs GhcPs LExpr))
      case e of
        GRHSs exts grhss localBinds ->
          GRHSs <$> goDo exts <*> goDo grhss <*> goNoDo localBinds

    -- Find all !s in the given statements and combine the resulting bind
    -- statements into listss, with the original statements being the last one
    -- in each list - then concatenate these lists
    addStmts :: MonadFill m => [ExprLStmt GhcPs] -> m [ExprLStmt GhcPs]
    addStmts = concatMapM \lstmt -> do
      (lstmt', stmts) <- runWriterT (goDo lstmt)
      pure $ (noLocA . fromBindStmt <$> stmts) ++ [lstmt']

type MonadFill m = (MonadWriter [BindStmt] m, MonadState FillState m)

data FillState = MkFillState
  { remainingErrors :: Map Loc LExpr
  }

data BindStmt = RdrName :<- LExpr

fromBindStmt :: BindStmt -> ExprStmt GhcPs
fromBindStmt (var :<- lexpr) = BindStmt EpAnnNotUsed varPat lexpr
  where
    varPat = noLocA . VarPat noExtField $ noLocA var

-- | Look up an error and remove it from the remaining errors if found
popError :: MonadFill m => Loc -> MaybeT m LExpr
popError loc = do
  remaining <- gets (.remainingErrors)
  let (merr, remainingErrors) = M.updateLookupWithKey (\_ _ -> Nothing) loc remaining
  putRemaining remainingErrors
  hoistMaybe merr
  where
    putRemaining remaining = modify \s -> s{remainingErrors = remaining}

mkVarName :: Loc -> RdrName
-- using spaces and special characters should make it impossible to overlap
-- with user-defined names (but could still technically overlap with names
-- introduced by other plugins)
mkVarName loc = mkVarUnqual . fsLit $ printf "<! from line %d, column %d>" loc.line loc.col

hoistMaybe :: Applicative m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure

(??) :: Functor f => f (a -> b) -> a -> f b
fs ?? x = ($ x) <$> fs
