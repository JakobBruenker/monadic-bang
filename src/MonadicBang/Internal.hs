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
import Control.Carrier.State.Strict
import Control.Carrier.Throw.Either
import Control.Carrier.Lift
import Control.Effect.Sum hiding (L)
import Control.Exception
import Control.Monad
import Data.Bifunctor
import Data.Bool
import Data.List (partition, intercalate)
import Data.Data
import Data.Maybe
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

data Verbosity = DumpTransformed | Quiet

data PreserveErrors = Preserve | Don'tPreserve

data Options = MkOptions {verbosity :: Verbosity, preserveErrors :: PreserveErrors}

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

parseOptions :: Has (Throw ErrorCall) sig m => Located HsModule -> [CommandLineOption] -> m Options
parseOptions mod' cmdLineOpts = do
  (remaining, options) <- runState cmdLineOpts do
    verbosity <- bool Quiet DumpTransformed <$> extractOpts verboseOpts
    preserveErrors <- bool Don'tPreserve Preserve <$> extractOpts preserveErrorsOpts
    pure $ MkOptions verbosity preserveErrors
  when (not $ null remaining) $ throw . ErrorCall $
    "Incorrect command line options for plugin MonadicBang, encountered in " ++ modName ++ modFile ++
    "\n\tOptions that were supplied (via -fplugin-opt) are: " ++ intercalate ", " (map show cmdLineOpts) ++
    "\n\tUnrecognized options: " ++ showOpts remaining ++
    "\n\n\tUsage: [-ddump] [-preserve-errors]" ++
    "\n" ++
    "\n\t\t-ddump            Print the altered AST" ++
    "\n\t\t-preserve-errors  Keep parse errors about ! outside of 'do' in their original form, rather then a more relevant explanation." ++
    "\n\t\t                  This is mainly useful if another plugin expects those errors."
  pure options

  where
    verboseOpts = ["-ddump"]
    preserveErrorsOpts = ["-preserve-errors"]
    extractOpts opt = do
      (isOpt, opts') <- gets $ first (not . null) . partition (`elem` opt)
      put opts'
      pure isOpt

    showOpts = intercalate ", " . map show

    modFile = fromMaybe "" $ ((" in file " ++) . unpackFS . srcSpanFile) <$> toRealSrcSpan (getLoc mod')
    modName = fromMaybe "an unnamed module" $ (("module " ++) . moduleNameString . unLoc) <$> (unLoc mod').hsmodName
    toRealSrcSpan = \cases
      (RealSrcSpan rss _) -> Just rss
      (UnhelpfulSpan _) -> Nothing

replaceBangs :: [CommandLineOption] -> ModSummary -> ParsedResult -> Hsc ParsedResult
replaceBangs cmdLineOpts _ (ParsedResult (HsParsedModule mod' files) msgs) = do
  options <- liftIO . (either throwIO pure =<<) . runThrow @ErrorCall $ parseOptions mod' cmdLineOpts
  traceShow cmdLineOpts $ pure ()
  (newErrors, mod'') <- runM . runUniquesIO 'p' . runWriter . runReader options $ fillHoles fills mod'
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
fillHoles :: (Data a, Has (PsErrors :+: Reader Options :+: Uniques) sig m) => Map Loc LExpr -> a -> m a
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
    evac e = maybe (gmapM evac $ e) pure =<< runMaybeT (asum $ [tryLExpr, tryStmt] ?? e)

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

    -- We use MaybeT since it has the MonadFail instance we want, as opposed to
    -- the other handlers for 'Empty'
    tryLExpr :: forall a sig m . (Has Fill sig m, Data a) => a -> MaybeT m a
    tryLExpr e = do
      Refl <- hoistMaybe (eqT @a @LExpr)
      ExprLoc loc expr <- pure e
      L l _ <- pure e
      case expr of
        -- Replace holes resulting from `!`
        -- If no corresponding expression can be found in the Offer, we assume
        -- that it was a hole put there by the user and leave it unmodified
        HsUnboundVar _ _ -> do
          lexpr' <- evac =<< MaybeT (yoink loc)
          name <- bangVar lexpr' loc
          tellOne (name :<- lexpr')
          evac . L l $  HsVar noExtField (noLocA name)
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

    -- | Find all !s in the given statements and combine the resulting bind
    -- statements into lists, with the original statements being the last one
    -- in each list - then concatenate these lists
    addStmts :: forall sig m . Has (PsErrors :+: HoleFills :+: Uniques) sig m => [ExprLStmt GhcPs] -> m [ExprLStmt GhcPs]
    addStmts = concatMapM \lstmt -> do
      (fromDList -> stmts, lstmt') <- runWriter (evac lstmt)
      pure $ map fromBindStmt stmts ++ [lstmt']

type DList a = Endo [a]

fromDList :: DList a -> [a]
fromDList = appEndo ?? []

tellOne :: Has (Writer (DList w)) sig m => w -> m ()
tellOne x = tell $ Endo (x:)

type PsErrors = Writer (Messages PsError)
type HoleFills = Offer Loc LExpr

type Fill = PsErrors :+: Writer (DList BindStmt) :+: HoleFills :+: Uniques

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

-- This is included in transformers 0.6, but that can't be used together with ghc 9.4
hoistMaybe :: Applicative m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure

tellPsError :: Has PsErrors sig m => PsError -> SrcSpan -> m ()
tellPsError err srcSpan = tell . singleMessage $ MsgEnvelope srcSpan neverQualify err SevError

(??) :: Functor f => f (a -> b) -> a -> f b
fs ?? x = ($ x) <$> fs

panic :: String -> a
panic message = error $ unlines ["MonadicBang panic:", message, "", submitReport]
  where
    submitReport = "This is likely a bug. Please submit a bug report under https://github.com/JakobBruenker/monadic-bang/issues"
