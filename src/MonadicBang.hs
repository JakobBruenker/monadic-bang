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
import Control.Monad.Writer.Strict
import Data.Data
import Data.Functor
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import GHC
import GHC.Data.Bag
import GHC.Parser.Errors.Types
import GHC.Plugins hiding (Expr)
import GHC.Types.Error
import Text.Printf

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
replaceBangs _ _ (ParsedResult (HsParsedModule lexp files) msgs) =
  pure $ ParsedResult (HsParsedModule (fillHoles fills lexp) files) msgs{psErrors}
  where
    -- Take out the errors we care about, throw the rest back in
    (mkMessages -> psErrors, M.fromList . bagToList -> fills) =
      flip partitionBagWith msgs.psErrors.getMessages \cases
        err | PsErrBangPatWithoutSpace (ExprLoc (addBang -> loc) expr) <- err.errMsgDiagnostic
            -> Right (loc, expr)
            | otherwise -> Left err

-- | Replace holes in an AST whenever an expression with the corresponding
-- source span can be found in the given list.
fillHoles :: Data a => Map Loc Expr -> a -> a
fillHoles fillers ast = case runState (goNoDo ast) (MkFillState fillers) of
  (ast', state') | null state'.remainingErrors -> ast'
                 | otherwise -> error "Found extraneous bangs" -- TODO improve error msg (incl. bug report url), possibly make an expression with custom type error signature so it will be shown as a proper error?
                                                                      -- But we might not be able to, since the parsed module usually doesn't import that... :(
  where

-- TODO: embed the expression in existing or new do-notation
-- Approach: in tryFillHole, whenever we encounter let/where/do/etc.,
-- make a separate monadic traversal through the subtree with a writer monad,
-- adding the bindings we need to the monadic context so we can construct the
-- correct do block once the traversal is evaluated

-- TODO: goNoDo shouldn't yet have the writer monad, only run the monads where it's actually necessary, don't need intercept
    goNoDo :: forall a m . (MonadState FillState m, Data a) => a -> m a
    goNoDo = gmapM \e -> maybe (goNoDo e) pure =<< runMaybeT (tryInsertDo e)

    -- surround the expression with a `do` if necessary
    tryInsertDo :: forall a m . (MonadState FillState m, Data a) => a -> MaybeT m a
    tryInsertDo expr = do
      Refl <- hoistMaybe (eqT @a @Expr)
      (expr', stmts) <- runWriterT (goDo expr)
      if null stmts
        then pure expr'
        else let lastStmt = BodyStmt noExtField (noLocA expr') noExtField noExtField
                 doStmts = (toHsStmt <$> stmts) ++ [lastStmt]
             in pure $ HsDo EpAnnNotUsed (DoExpr Nothing) (noLocA $ noLocA <$> doStmts)
     where
       toHsStmt :: BindStmt -> ExprStmt GhcPs
       toHsStmt (var :<- boundExpr) = BindStmt EpAnnNotUsed varPat lexpr
        where
          varPat = noLocA . VarPat noExtField $ noLocA var
          lexpr = noLocA boundExpr

    goDo :: forall a m . (MonadFill m, Data a) => a -> m a
    goDo = gmapM \e -> maybe (goDo e) pure =<< runMaybeT (tryExpr e)

    tryExpr :: forall a m . (MonadFill m, Data a) => a -> MaybeT m a
    tryExpr e = do
      Refl <- hoistMaybe (eqT @a @LExpr)
      lexp@(ExprLoc loc (HsUnboundVar _ _)) <- pure e
      expr <- popError loc
      let name = mkVarName loc
      tell [name :<- expr]
      goDo $ lexp $> HsVar noExtField (noLocA name)

type MonadFill m = (MonadWriter [BindStmt] m, MonadState FillState m)

data FillState = MkFillState
  { remainingErrors :: Map Loc Expr
  }

data BindStmt = RdrName :<- Expr

-- | Look up an error and remove it from the remaining errors if found
popError :: MonadFill m => Loc -> MaybeT m Expr
popError loc = do
  remaining <- getRemaining
  let (merr, remaining') = M.updateLookupWithKey (\_ _ -> Nothing) loc remaining
  putRemaining remaining'
  hoistMaybe merr
  where
    getRemaining = gets \s -> s.remainingErrors
    putRemaining remaining = modify \s -> s{remainingErrors = remaining}

mkVarName :: Loc -> RdrName
-- using spaces and ! should make it impossible to overlap with user-defined
-- names (but could still technically overlap with names introduced by other
-- plugins)
mkVarName loc = mkVarUnqual . fsLit $ printf "! in line %d, column %d" loc.line loc.col

hoistMaybe :: Applicative m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure
