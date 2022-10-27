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
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}

module MonadicBang (plugin) where

import Prelude hiding (log)
import Control.Applicative
import Control.Algebra (send, alg)
import Control.Monad.Trans.Maybe
import Control.Carrier.Reader
import Control.Carrier.Writer.Strict
import Control.Carrier.State.Strict
import Control.Carrier.Throw.Either
import Control.Effect.Sum hiding (L)
import Control.Effect.Sum qualified as Sum
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
import GHC.Plugins hiding (Expr, Let, empty, (<>), panic)
import GHC.Types.Error
import GHC.Utils.Monad (concatMapM)
import Text.Printf

import Debug.Trace
-- import GHC.Hs.Dump -- XXX JB

import GHC.Utils.Logger

-- TODO: Write user manual as haddock comment

-- TODO: mention in the documentation how unfortunately you get a parse error for each exclamation mark if you get a fatal parse error

plugin :: Plugin
plugin = defaultPlugin
  { parsedResultAction = replaceBangs
  -- , pluginRecompile = purePlugin -- XXX JB the plugin is pure, just commenting it out so we can see the output every time we run the tests
  }

data Verbosity = Verbose | Quiet

data PreserveErrors = Preserve | Don'tPreserve

data Options = MkOptions {verbosity :: Verbosity, preserveErrors :: PreserveErrors}

-- We don't care about which file things are from, because the entire AST comes
-- from the same module
data Loc = MkLoc {line :: Int, col :: Int}
         deriving (Eq, Ord, Show)

type Expr = HsExpr GhcPs
type LExpr = LHsExpr GhcPs

-- | Decrement column by one to get the location of a bang
bangLoc :: Loc -> Loc
bangLoc loc = loc{col = loc.col - 1}

-- | Decrement start by one column to get the location of a bang
bangSpan :: SrcSpan -> SrcSpan
bangSpan sp = mkSrcSpan (bangSrcLoc $ srcSpanStart sp) (srcSpanEnd sp)

-- | Decrement column by one to get the location of a bang
bangSrcLoc :: SrcLoc -> SrcLoc
bangSrcLoc = \cases
  l@(UnhelpfulLoc _) -> l
  (RealSrcLoc srcLoc _) -> liftA3 mkSrcLoc srcLocFile srcLocLine (pred . srcLocCol) srcLoc

-- | Used to extract the Loc of a located expression
pattern ExprLoc :: Loc -> Expr -> LExpr
pattern ExprLoc loc expr <- L (locA -> RealSrcSpan (spanToLoc -> loc) _) expr

spanToLoc :: RealSrcSpan -> Loc
spanToLoc = liftA2 MkLoc srcLocLine srcLocCol . realSrcSpanStart

-- Offers a number of things that can be yoinked, but only once
data Offer k v m a where
  Yoink :: k -> Offer k v m (Maybe v)

yoink :: (Has (Offer k v) sig m) => k -> m (Maybe v)
yoink = send . Yoink

newtype OfferC k v m a = OfferC {getOfferState :: StateC (Map k v) m a}
  deriving newtype (Functor, Applicative, Monad)

-- Returns the result of the computation, along with the remaining offers
runOffer :: Map k v -> OfferC k v m a -> m (Map k v, a)
runOffer o (OfferC s) = runState o s

instance (Algebra sig m, Ord k) => Algebra (Offer k v :+: sig) (OfferC k v m) where
  alg hdl sig ctx = case sig of
    Sum.L (Yoink k) -> OfferC do
      (mv, remaining) <- M.updateLookupWithKey (\_ _ -> Nothing) k <$> get
      put remaining
      pure (mv <$ ctx)
    Sum.R other -> OfferC (alg ((.getOfferState) . hdl) (Sum.R other) ctx)

parseOptions :: Has (Throw ErrorCall) sig m => Located HsModule -> [CommandLineOption] -> m Options
parseOptions mod' cmdLineOpts = do
  (remaining, options) <- runState cmdLineOpts do
    verbosity <- bool Quiet Verbose <$> extractOpts verboseOpts
    preserveErrors <- bool Don'tPreserve Preserve <$> extractOpts preserveErrorsOpts
    pure $ MkOptions verbosity preserveErrors
  when (not $ null remaining) $ throw . ErrorCall $
    "Incorrect command line options for plugin MonadicBang, encountered in " ++ modName ++ modFile ++
    "\n\tOptions that were supplied (via -fplugin-opt) are: " ++ intercalate ", " (map show cmdLineOpts) ++
    "\n\tUnrecognized options: " ++ showOpts remaining ++
    "\n\n\tUsage: [-v|--verbose] [--preserve-errors]" ++
    "\n" ++
    "\n\t\t-v --vebose           Print the altered AST" ++
    "\n\t\t   --preserveErrors   Keep parse errors about ! outside of 'do' in their original form, rather then a more relevant explanation." ++
    "\n\t\t                      This is mainly useful if another plugin that expects those errors."
  pure options

  where
    verboseOpts = ["-v", "--verbose"]
    preserveErrorsOpts = ["--preserve-errors"]
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
  -- trace (showSDocUnsafe $ showAstDataFull mod') $ -- XXX JB
  let (newErrors, mod'') = run . runWriter . runReader options $ fillHoles fills mod'
  log options.verbosity (ppr mod'')
  pure $ ParsedResult (HsParsedModule mod'' files) msgs{psErrors = oldErrors <> newErrors}
  where
    log = \cases
      Quiet _ -> pure ()
      Verbose m -> do
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
fillHoles :: (Data a, Has (PsErrors :+: Reader Options) sig m) => Map Loc LExpr -> a -> m a
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
        -- TODO: !_ doesn't work, I think we have to make sure here only to try to fill it if it's actually a bang error
        HsUnboundVar _ _ -> do
          lexpr' <- evac =<< fromMaybe (panic "Couldn't find hole filler") <$> yoink loc -- maybe improve error message
          let name = bangVar lexpr' loc
          tellOne (name :<- lexpr')
          evac . L l $  HsVar noExtField (noLocA name)
        HsDo xd ctxt stmts -> L l . HsDo xd ctxt <$> traverse addStmts stmts
        _ -> empty

    -- | Find all !s in the given statements and combine the resulting bind
    -- statements into lists, with the original statements being the last one
    -- in each list - then concatenate these lists
    addStmts :: forall sig m . Has (PsErrors :+: HoleFills) sig m => [ExprLStmt GhcPs] -> m [ExprLStmt GhcPs]
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

type Fill = PsErrors :+: Writer (DList BindStmt) :+: HoleFills

data BindStmt = RdrName :<- LExpr
              -- let var param1 ... paramn = val
              -- XXX JB I don't think we need this anymore
              | Let { var :: RdrName
                    , params :: [RdrName]
                    , val :: LExpr
                    }

bindStmtExpr :: BindStmt -> LExpr
bindStmtExpr = \cases
  (_ :<- expr) -> expr
  Let{val = expr} -> expr

bindStmtSpan :: BindStmt -> SrcSpan
bindStmtSpan = (.locA) . \cases
  (_ :<- L l _) -> l
  Let{val = L l _} -> l

fromBindStmt :: BindStmt -> ExprLStmt GhcPs
fromBindStmt = noLocA . \cases
  (var :<- lexpr) -> BindStmt EpAnnNotUsed varPat lexpr
    where
      varPat = noLocA . VarPat noExtField $ noLocA var
  Let{var, params, val} -> LetStmt EpAnnNotUsed $ binding
    where
      lvar = noLocA var
      binding = HsValBinds EpAnnNotUsed valBinds
      valBinds = ValBinds NoAnnSortKey (unitBag . noLocA $ FunBind noExtField lvar mg []) []
      mg = MG noExtField (noLocA [noLocA match]) Generated
      pats = noLocA . VarPat noExtField . noLocA <$> params
      match = Match EpAnnNotUsed (FunRhs lvar GHC.Prefix NoSrcStrict) pats . GRHSs emptyComments [rhs] $
        EmptyLocalBinds noExtField
      rhs = noLocA $ GRHS EpAnnNotUsed [] val

-- Use the !'d expression if it's short enough, or else just <!expr>
-- We don't need to worry about shadowing, since we add the line and column numbers
bangVar :: LExpr -> Loc -> RdrName
bangVar (L _ expr) = locVar . ('!':) $ case lines (showPprUnsafe expr) of
  (str:rest) | null rest && length str < 20 -> str
             | otherwise                    -> take 16 str ++ "..."
  _                                         -> "<empty expression>"

locVar :: String -> Loc -> RdrName
-- using spaces and special characters should make it impossible to overlap
-- with user-defined names (but could still technically overlap with names
-- introduced by other plugins)
-- TODO is there a way to make a RdrName that's guaranteed to be unique, but has this as OccName? Maybe nameRdrName with mkInternalName
-- TODO however you need a unique for that, and all the ways I can see to make uniques are determinstic, so not sure they would actually be "unique"
-- TODO unless UniqSupply works?
locVar str loc = mkVarUnqual . fsLit $
  printf "<%s:%d:%d>" str loc.line loc.col

-- This is included in transformers 0.5, but that can't be used together with ghc 9.4
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
