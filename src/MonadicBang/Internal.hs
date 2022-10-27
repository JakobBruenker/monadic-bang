{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingStrategies #-}

module MonadicBang.Internal where

import Prelude hiding (log)
import Control.Applicative
import Control.Monad.Trans.Maybe
import Control.Carrier.Reader
import Control.Carrier.Writer.Strict
import Control.Carrier.Throw.Either
import Control.Carrier.Lift
import Control.Effect.Sum hiding (L)
import Control.Exception
import Data.Data
import Data.Foldable
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Monoid
import GHC
import GHC.Data.Bag
import GHC.Parser.Errors.Types
import GHC.Plugins hiding (Expr, empty, (<>), panic)
import GHC.Types.Error
import GHC.Utils.Monad (concatMapM)
import Text.Printf

import Debug.Trace

import GHC.Utils.Logger

import MonadicBang.Effect.Offer
import MonadicBang.Effect.Uniques
import MonadicBang.Options
import MonadicBang.Utils

-- We don't care about which file things are from, because the entire AST comes
-- from the same module
data Loc = MkLoc {line :: Int, col :: Int}
         deriving (Eq, Ord, Show)

type Expr = HsExpr GhcPs
type LExpr = LHsExpr GhcPs

-- | Decrement column by one to get the location of a !
bangLoc :: Loc -> Loc
bangLoc loc = loc{col = loc.col - 1}

-- | Decrement start by one column to get the location of a !
bangSpan :: SrcSpan -> SrcSpan
bangSpan sp = mkSrcSpan (bangSrcLoc $ srcSpanStart sp) (srcSpanEnd sp)

-- | Decrement column by one to get the location of a !
bangSrcLoc :: SrcLoc -> SrcLoc
bangSrcLoc = \cases
  l@(UnhelpfulLoc _) -> l
  (RealSrcLoc srcLoc _) -> liftA3 mkSrcLoc srcLocFile srcLocLine (pred . srcLocCol) srcLoc

-- | Used to extract the Loc of a located expression
pattern ExprLoc :: Loc -> Expr -> LExpr
pattern ExprLoc loc expr <- L (locA -> RealSrcSpan (spanToLoc -> loc) _) expr

spanToLoc :: RealSrcSpan -> Loc
spanToLoc = liftA2 MkLoc srcLocLine srcLocCol . realSrcSpanStart

replaceBangs :: [CommandLineOption] -> ModSummary -> ParsedResult -> Hsc ParsedResult
replaceBangs cmdLineOpts _ (ParsedResult (HsParsedModule mod' files) msgs) = do
  options <- liftIO . (either throwIO pure =<<) . runThrow @ErrorCall $ parseOptions mod' cmdLineOpts
  traceShow cmdLineOpts $ pure ()
  (newErrors, mod'') <- runM . runUniquesIO 'p' . runWriter . runReader options . runReader emptyOccSet $ fillHoles fills mod'
  log options.verbosity (ppr mod'')
  pure $ ParsedResult (HsParsedModule mod'' files) msgs{psErrors = oldErrors <> newErrors}
  where
    log = \cases
      Quiet _ -> pure ()
      DumpTransformed m -> do
        logger <- getLogger
        liftIO $ logMsg logger MCInfo (UnhelpfulSpan UnhelpfulNoLocationInfo) m

    -- Extract the errors we care about, throw the rest back in
    (mkMessages -> oldErrors, M.fromList . bagToList -> fills) =
      (partitionBagWith ?? msgs.psErrors.getMessages) \cases
        err | PsErrBangPatWithoutSpace lexpr@(ExprLoc (bangLoc -> loc) _) <- err.errMsgDiagnostic
            -> Right (loc, lexpr)
            | otherwise -> Left err

-- | Replace holes in an AST whenever an expression with the corresponding
-- source span can be found in the given list.
fillHoles :: (Data a, Has (PsErrors :+: Reader Options :+: Uniques :+: LocalVars) sig m) => Map Loc LExpr -> a -> m a
fillHoles fillers ast = do
  (remainingErrs, (fromDList -> binds :: [BindStmt], ast')) <- runOffer fillers . runWriter $ evac ast
  MkOptions{preserveErrors} <- ask
  for_ binds \bind -> tellPsError (psError (bindStmtExpr bind) preserveErrors) (bangSpan $ bindStmtSpan bind)
  pure if null remainingErrs
    then ast'
    else panic $ "Found extraneous bangs:" ++ unlines (showSDocUnsafe . ppr <$> toList remainingErrs)
  where
    psError expr = \cases
      Preserve      -> PsErrBangPatWithoutSpace expr
      Don'tPreserve -> PsUnknownMessage $ DiagnosticMessage
        { diagMessage = mkDecorated [text "Monadic ! outside of a 'do'-block is not allowed"]
        , diagReason = ErrorWithoutFlag
        , diagHints = [SuggestMissingDo]
        }

    evac :: forall a sig m . (Has Fill sig m, Data a) => a -> m a
    -- This recurses over all nodes in the AST, except for nodes for which
    -- one of the `try` functions returns `Just <something>`.
    -- TODO: Via benchmarking, find out whether it makes sense to `try` more
    -- datatypes here (e.g. `tryRdrName`) that would always remain unmodified
    -- anyway due to not containing expressions, and thus don't need to be
    -- recursed over
    evac e = maybe (gmapM evac e) pure =<< runMaybeT (asum $ [tryLExpr, tryStmt] ?? e)

    -- We use MaybeT since it has the MonadFail instance we want, as opposed to
    -- the other handlers for 'Empty'
    tryLExpr :: forall a sig m . (Has Fill sig m, Data a) => a -> MaybeT m a
    tryLExpr e = do
      Refl <- hoistMaybe (eqT @a @LExpr)
      ExprLoc loc expr <- pure e
      let L l _ = e
      case expr of
        -- Replace holes resulting from `!`
        -- If no corresponding expression can be found in the Offer, we assume
        -- that it was a hole put there by the user and leave it unmodified
        HsUnboundVar _ _ -> do
          yoink loc >>= maybe (pure e) \lexpr -> do
            lexpr' <- evac lexpr
            name <- bangVar lexpr' loc
            tellOne $ name :<- lexpr'
            evac . L l $ HsVar noExtField (noLocA name)
        HsDo xd ctxt stmts -> L l . HsDo xd ctxt <$> traverse addStmts stmts

        -- the remaining cases are only added to aid in performance, to avoid
        -- recursing over their constructor arguments, which don't contain
        -- expressions anyway
        HsVar{} -> pure e
        HsRecSel{} -> pure e
        HsOverLabel{} -> pure e
        HsIPVar{} -> pure e
        HsOverLit{} -> pure e
        HsLit{} -> pure e
        HsProjection{} -> pure e

        _ -> empty

    tryStmt :: forall a sig m . (Has Fill sig m, Data a) => a -> MaybeT m a
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

    -- | Find all !s in the given statements and combine the resulting bind
    -- statements into lists, with the original statements being the last one
    -- in each list - then concatenate these lists
    addStmts :: forall sig m . Has (PsErrors :+: HoleFills :+: Uniques :+: LocalVars) sig m => [ExprLStmt GhcPs] -> m [ExprLStmt GhcPs]
    addStmts = concatMapM \lstmt -> do
      (fromDList -> stmts, lstmt') <- runWriter (evac lstmt)
      pure $ map fromBindStmt stmts ++ [lstmt']

type PsErrors = Writer (Messages PsError)
type HoleFills = Offer Loc LExpr
-- | We keep track of variables that are bound in lambdas, cases, etc., since
-- these are variables that will not be accessible in the surrounding
-- 'do'-block, and must therefore not be used
type LocalVars = Reader OccSet

type Fill = PsErrors :+: Writer (DList BindStmt) :+: HoleFills :+: Uniques :+: LocalVars

data BindStmt = RdrName :<- LExpr

bindStmtExpr :: BindStmt -> LExpr
bindStmtExpr (_ :<- expr) = expr

bindStmtSpan :: BindStmt -> SrcSpan
bindStmtSpan = (.locA) . \(_ :<- L l _) -> l

fromBindStmt :: BindStmt -> ExprLStmt GhcPs
fromBindStmt = noLocA . \cases
  (var :<- lexpr) -> BindStmt EpAnnNotUsed varPat lexpr
    where
      varPat = noLocA . VarPat noExtField $ noLocA var

-- Use the !'d expression if it's short enough, or else just <!expr>
-- We don't need to worry about shadowing other !'d expressions, since we add
-- the line and column numbers
bangVar :: Has Uniques sig m => LExpr -> Loc -> m RdrName
bangVar (L spn expr) = (locVar . ('!':)) ?? spn.locA $ case lines (showPprUnsafe expr) of
  (str:rest) | null rest && length str < 20 -> str
             | otherwise                    -> take 16 str ++ "..."
  _                                         -> "<empty expression>"

locVar :: Has Uniques sig m => String -> SrcSpan -> Loc -> m RdrName
locVar str spn loc = do
  let occ = mkVarOcc $ printf "<%s:%d:%d>" str loc.line loc.col
  unique <- freshUnique
  pure . nameRdrName $ mkInternalName unique occ spn

tellPsError :: Has PsErrors sig m => PsError -> SrcSpan -> m ()
tellPsError err srcSpan = tell . singleMessage $ MsgEnvelope srcSpan neverQualify err SevError

tellOne :: Has (Writer (DList w)) sig m => w -> m ()
tellOne x = tell $ Endo (x:)
