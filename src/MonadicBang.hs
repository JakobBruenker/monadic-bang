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
import Data.List.NonEmpty (nonEmpty)
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

-- TODO split into modules

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

yoink :: Has (Offer k v) sig m => k -> m (Maybe v)
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

-- TODO: Maybe control with a pragma whether holes outside of `do`s are caught and replaced with a better error message referencing the plugin, or left along
-- TODO should the fancy case/if behavior be controlled via command line flag? I.e. otherwise you get Idris-like behavior
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
  -- trace (showSDocUnsafe $ showAstData BlankSrcSpan BlankEpAnnotations mod') $ -- XXX JB
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
  pure case nonEmpty (toList remainingErrs) of
    Nothing -> ast'
    -- Just neErrs -> panic "Found extraneous bangs" -- TODO improve error msg?
    --                                               -- Use PsUnknownMessage? (maybe not since this is a panic)
    Just _neErrs -> trace "XXX JB WARNING" ast'
  where
    psError expr = \cases
      Preserve      -> PsErrBangPatWithoutSpace expr
      Don'tPreserve -> PsUnknownMessage $ DiagnosticMessage
        { diagMessage = mkDecorated [text "Monadic ! outside of a 'do'-block is not allowed"]
        , diagReason = ErrorWithoutFlag
        , diagHints = [SuggestMissingDo]
        }

    evac :: forall a sig m . (Has Fill sig m, Data a) => a -> m a
    evac e = maybe (gmapM evac $ e) pure =<< runMaybeT (asum $ [tryLExpr, tryStmt, tryGRHSs] ?? e)

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
        HsUnboundVar _ _ -> do
          lexpr' <- evac =<< fromMaybe (panic "Couldn't find hole filler") <$> yoink loc -- maybe improve error message
          let name = bangVar lexpr' loc
          tellOne (name :<- lexpr')
          evac . L l $  HsVar noExtField (noLocA name)
        -- For case, we only bind the actions in the alternative that is
        -- actually chosen - as well as any actions used in view patterns or
        -- guards for that or previous alternatives
        -- TODO: The HsDo constructed by this should take into account whether or not we're inside a qualifiedDo
        --       by putting the module name inside reader
        -- TODO: ideally we only want to transform HsCase when it's actually necessary
        --       by writing to a different writer (Any newtype) that records whether or not we have to
        -- TODO: If this case expression is a statement in a do block, we don't need to do any transformations just the idris treatment is enough
        --       However, the same is true if it's just preceded by "id" or more generally has a monadic type, so we can't handle this consistently anyway, so maybe we just always want to treat it the same?
        --       Especially when you keep in mind that even inside do blocks, you can actually have non-monadic stuff (e.g. "do case () of () -> 4") EXCEPT! That's not true anymore as soon as you use ! inside the do
        --       see TODO [case stmt] for more questionable circumstances
        -- TODO: Technically, you could also split up nested patterns, such that in (Just (!foo -> X)), !foo is only executed in the Just case
        --       seems doable
        -- TODO: we might get unused warnings with the scrutinee if we're not careful. I guess we'll have to traverse the tree to see if the scrutinee is being used...?
        -- TODO: Wait, can we just prefix it with _ to prevent such warnings?
        -- HsCase xc scrut mg -> do
        --   scrut' <- evac scrut
        --   (lambdas, mg') <- evacMatchGroup mg
        --   let binds = locVar "binds for case" loc
        --       bindsExpr = L l $ HsCase xc scrut' mg'
        --       bindsVar = noLocA . HsVar noExtField $ noLocA binds
        --   tellOne (binds :<- bindsExpr)
        -- If we encounter a `do`, use that instead of a `do` we inserted
        HsDo xd ctxt stmts -> L l . HsDo xd ctxt <$> traverse addStmts stmts
        _ -> empty


--     evacMatchGroup :: Has Fill sig m => MatchGroup GhcPs LExpr -> m ([LExpr], MatchGroup GhcPs LExpr)
--     evacMatchGroup mg@MG{mg_alts} = do
--       let L l alts = mg_alts
--       (binds, alts') <- evacAlts alts
--       traverse_ tellOne binds
--       pure ([], mg{mg_alts = L l alts'})
--       where
--         -- Returns any binds from the first alternative as well as the modified matches
--         evacAlts :: forall sig m . Has (State Errors) sig m => [LMatch GhcPs LExpr] -> m ([BindStmt], [LMatch GhcPs LExpr])
--         evacAlts (lmatch : lmatches) = do
--           (nonEmpty . fromDList -> firstAltBinds, pats') <- runWriter $ evacAlt lmatch
--           error "TODO~"
--         -- evacAlts [] = pure ([], [])
--         -- evacAlts (L l match@Match{m_pats, m_ctxt, m_grhss} : lmatches) = do
--         --   (fromDList -> firstAltBinds, pats') <- runWriter $ evac m_pats
--         --   localBinds' <- lookForDo grhssLocalBinds
--         --   (binds, lmatches') <- evacAlts lmatches
--         --   evacLGRHSs grhssGRHSs >>= \cases
--         --     (Right grhss') -> do
--         --       let lmatch' = L l match{m_pats = pats', m_grhss = m_grhss{grhssGRHSs = grhss', grhssLocalBinds = localBinds'}}
--         --       case nonEmpty binds of -- TODO even though we need a regular list this is actually still safer if we use nonEmpty and toList
--         --         Nothing -> pure (firstAltBinds, lmatch' : lmatches')
--         --         Just neBinds -> do
--         --           -- Example of what's happenning here:
--         --           --   case s of
--         --           --     a -> ...
--         --           --     (!b -> X) -> ...
--         --           --     c -> ...
--         --           -- becomes
--         --           --   case s of
--         --           --     a -> ...
--         --           --     <scrutinee> -> do
--         --           --       <!b> <- b
--         --           --       case <scrutinee> of
--         --           --         (<!b> -> X) -> ...
--         --           --         c -> ...
--         --           scrutVar <- newScrutVar
--         --           let newMG = MG noExtField (noLocA lmatches') Generated
--         --               newCase = HsCase EpAnnNotUsed (noLocA $ HsVar noExtField $ noLocA scrutVar) newMG
--         --               newStmt = BodyStmt noExtField (noLocA newCase) noExtField noExtField
--         --               newExpr = HsDo EpAnnNotUsed (DoExpr Nothing) (noLocA $ (fromBindStmt <$> toList neBinds) ++ [noLocA newStmt])
--         --               newGRHS = GRHS EpAnnNotUsed [] (noLocA newExpr)
--         --               newGRHSs = GRHSs emptyComments [noLocA newGRHS] emptyLocalBinds
--         --               newMatch = noLocA $ Match EpAnnNotUsed m_ctxt [noLocA . VarPat noExtField $ noLocA scrutVar] newGRHSs
--         --           pure (firstAltBinds, [lmatch', newMatch])
--         --     (Left _) -> error "TODO Left"

--         evacAlt :: forall sig m . Has (State Errors) sig m => LMatch GhcPs LExpr -> m ([BindStmt], [LMatch GhcPs LExpr])
--         evacAlt (L l Match{m_pats, m_ctxt, m_grhss}) = do
--           error "TODO alt"
--           -- (nonEmpty . fromDList -> binds, remainingPats) <- runWriter $ evacPat pat

--         evacPat :: forall sig m . Has Fill sig m => LPat GhcPs -> m (Either (LPat GhcPs, [([BindStmt], LPat GhcPs, RdrName)]) (LPat GhcPs))
--         evacPat lpat@(L l pat) = case pat of
--           (WildPat _) -> pure $ Right lpat
--           (VarPat _ _) -> pure $ Right lpat
--           (LazyPat xp p) -> L l . LazyPat xp <$$$> evacPat p
--           (AsPat xp i p) -> L l . AsPat xp i <$$$> evacPat p
--           (ParPat xp lt p rt) -> L l . (ParPat xp lt ?? rt) <$$$> evacPat p
--           (BangPat xp p) -> L l . BangPat xp <$$$> evacPat p
--           (ListPat xp ps) -> do
--             (ps', withBangs) <- traverseUntilLeft evacPat ps
--             case withBangs of
--               Nothing -> pure $ Right lpat
--               -- Oh boy I don't even know. I don't think checking rest is what we have to do here, we have to check whether there's any patterns that were produced by the first pat that contain bangs, or if rest contains bangs
--               -- feel like there's a better approach here but ugh I don't know what it is
--               Just (firstBang, rest) -> case nonEmpty rest of
--           where
--             infixl 4 <$$$>
--             (<$$$>) = (fmap . first . first)
--           --   (Left _) -> error "TODO Left"

--         -- If there is at least one guard that requires bind statements, it it
--         -- returns those, and the modified GRHSs up to that point, and the
--         -- guards up to that point, and the unmodified GRHS without that guard,
--         -- and the remaining unmodified GRHSs
--         -- Otherwise returns the modified GRHSs
--         evacLGRHSs :: forall sig m . Has (State Errors) sig m => [LGRHS GhcPs LExpr] ->
--           m (Either ([BindStmt], [LGRHS GhcPs LExpr], [GuardLStmt GhcPs], LGRHS GhcPs LExpr, [LGRHS GhcPs LExpr]) [LGRHS GhcPs LExpr])
--         evacLGRHSs grhss = do
--           (grhss', rest) <- untilLeft evacLGRHS grhss
--           case rest of
--             Nothing -> pure $ Right grhss'
--             Just ((stmts, guards, grhs), grhssRest) -> pure $ Left (stmts, grhss', guards, grhs, grhssRest)

--         -- If there is at least one guard that requires bind statements, it
--         -- returns those, and the guards up to that point, and the unmodified
--         -- GRHS without those guards
--         -- Otherwise returns the modified GRHS
--         evacLGRHS :: forall sig m . Has (State Errors) sig m => LGRHS GhcPs LExpr ->
--           m (Either ([BindStmt], [GuardLStmt GhcPs], LGRHS GhcPs LExpr) (LGRHS GhcPs LExpr))
--         evacLGRHS lgrhs@(L l (GRHS xg guards body)) = case guards of
--           [] -> pure $ Right lgrhs -- TODO replace holes and make sum type and what not
--           g:gs -> do
--             (fromDList -> stmts, g') <- runWriter $ evac g
--             case nonEmpty stmts of
--               Nothing -> addGuard g' <$> evacLGRHS (L l $ GRHS xg gs body)
--               Just neStmts -> pure $ Left (toList neStmts, [g'], L l $ GRHS xg gs body)
--           where
--             addGuard g = \cases
--               (Left (stmts, gs, rhs)) -> Left (stmts, g:gs, rhs)
--               (Right (L l' (GRHS @GhcPs xg' gs body'))) -> Right (L l' $ GRHS xg' (g:gs) body')

--         untilLeft :: forall m a b e . Monad m => (a -> m (Either e b)) -> [a] -> m ([b], Maybe (e, [a]))
--         untilLeft f = (first reverse <$>) . go []
--           where
--             go acc [] = pure (acc, Nothing)
--             go acc (x:xs) = f x >>= \cases
--               (Left e) -> pure (acc, Just (e, xs))

    -- This lets us start new do-blocks in where blocks
    tryGRHSs :: forall a sig m . (Has Fill sig m, Data a) => a -> MaybeT m a
    tryGRHSs e = do
      Refl <- hoistMaybe (eqT @a @(GRHSs GhcPs LExpr))
      GRHSs exts grhss localBinds <- pure e
      GRHSs exts <$> evac grhss <*> evac localBinds

    -- | Find all !s in the given statements and combine the resulting bind
    -- statements into lists, with the original statements being the last one
    -- in each list - then concatenate these lists
    addStmts :: forall sig m . Has (PsErrors :+: HoleFills) sig m => [ExprLStmt GhcPs] -> m [ExprLStmt GhcPs]
    addStmts = concatMapM \lstmt -> do
      (fromDList -> stmts, lstmt') <- runWriter (evac lstmt)
      pure $ (fromBindStmt <$> stmts) ++ [lstmt']

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
locVar str loc = mkVarUnqual . fsLit $
  printf "<%s:%d:%d>" str loc.line loc.col

-- TODO use Fresh to avoid shadowing
-- XXX JB unneeded?
-- newScrutVar :: Monad m => m RdrName
-- newScrutVar = pure . mkVarUnqual . fsLit $ "<scrutinee>"

-- This is included in transformers 0.5, but that can't be used together with ghc 9.4
hoistMaybe :: Applicative m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure

tellPsError :: Has PsErrors sig m => PsError -> SrcSpan -> m ()
tellPsError err srcSpan = tell . singleMessage $ MsgEnvelope srcSpan neverQualify err SevError

-- traverseUntilLeft :: Monad f => (a -> f (Either b c)) -> [a] -> f ([c], Maybe (b, [a]))
-- traverseUntilLeft f = fix \go -> \cases
--   [] -> pure ([], Nothing)
--   (x:xs) -> f x >>= \cases
--     (Left b) -> pure ([], Just (b, xs))
--     (Right c) -> first (c:) <$> go xs

-- traverseToFst :: Functor f => (a -> f b) -> a -> f (b, a)
-- traverseToFst f x = (, x) <$> f x

(??) :: Functor f => f (a -> b) -> a -> f b
fs ?? x = ($ x) <$> fs

-- TODO add instruction to send bug report
panic :: String -> a
panic = error
