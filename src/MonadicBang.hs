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

module MonadicBang (plugin) where

import Prelude hiding (log)
import Control.Applicative
import Control.Monad.Trans.Maybe
import Control.Carrier.Writer.Strict
import Control.Carrier.State.Strict
import Control.Effect.Sum hiding (L)
import Control.Exception
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
import GHC.Plugins hiding (Expr, Let, empty, (<>))
import GHC.Types.Error
import GHC.Utils.Monad (concatMapM)
import Text.Printf

import Debug.Trace
-- import GHC.Hs.Dump -- XXX JB

import GHC.Utils.Logger

-- TODO: Write user manual as haddock comment

plugin :: Plugin
plugin = defaultPlugin
  { parsedResultAction = replaceBangs
  -- , pluginRecompile = purePlugin -- XXX JB the plugin is pure, just commenting it out so we can see the output every time we run the tests
  }

data Verbosity = Verbose | Quiet

-- We don't care about which file things are from, because the entire AST comes
-- from the same module
data Loc = MkLoc {line :: Int, col :: Int}
         deriving (Eq, Ord, Show)

type Expr = HsExpr GhcPs
type LExpr = LHsExpr GhcPs

-- | Decrement column by one to get the location of a bang
bangLoc :: Loc -> Loc
bangLoc loc = loc{col = loc.col - 1}

-- | Used to extract the Loc of a located expression
pattern ExprLoc :: Loc -> Expr -> LExpr
pattern ExprLoc loc expr <- L (locA -> RealSrcSpan (spanToLoc -> loc) _) expr

spanToLoc :: RealSrcSpan -> Loc
spanToLoc = liftA2 MkLoc srcLocLine srcLocCol . realSrcSpanStart

parseOptions :: Located HsModule -> [CommandLineOption] -> Either ErrorCall Verbosity
parseOptions mod' options = case options of
  ["verbose"] -> pure Verbose
  ["v"] -> pure Verbose
  ["quiet"] -> pure Quiet
  ["q"] -> pure Quiet
  [] -> pure Quiet
  _ -> Left . ErrorCall $
    "Incorrect command line options for plugin MonadicBang, encountered in " ++ modName ++ modFile ++
    "\n\tOptions that were supplied are " ++ show options ++
    "\n\n\tUsage: Supply a single argument chosen from v[erbose] | q[uiet]" ++
    "\n\tThe default if no argument is supplied is \"quiet\""

  where
    modFile = fromMaybe "" $ ((" in file " ++) . unpackFS . srcSpanFile) <$> toRealSrcSpan (getLoc mod')
    modName = fromMaybe "an unnamed module" $ (("module " ++) . moduleNameString . unLoc) <$> (unLoc mod').hsmodName

    toRealSrcSpan = \cases
      (RealSrcSpan rss _) -> Just rss
      (UnhelpfulSpan _) -> Nothing

replaceBangs :: [CommandLineOption] -> ModSummary -> ParsedResult -> Hsc ParsedResult
replaceBangs options _ (ParsedResult (HsParsedModule mod' files) msgs) = do
  verbosity <- liftIO . either throwIO pure $ parseOptions mod' options
  traceShow options $ pure ()
  -- trace (showSDocUnsafe $ showAstData BlankSrcSpan BlankEpAnnotations mod') $ -- XXX JB
  let (newErrors, mod'') = run . runWriter $ fillHoles fills mod'
  log verbosity (ppr mod'')
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
fillHoles :: (Data a, Has (PsErrors) sig m) => Bang'dExprs -> a -> m a
fillHoles fillers ast = do
  (remainingErrs, ast') <- runState fillers $ evacWithNewDo ast
  pure case nonEmpty (toList remainingErrs) of
    Nothing -> ast'
    -- Just neErrs -> error "Found extraneous bangs" -- TODO improve error msg (incl. bug report url)
    --                                               -- Use PsUnknownMessage? (maybe not since this is a panic)
    Just _neErrs -> trace "XXX JB WARNING" ast'
  where
    evacWithNewDo :: forall a sig m . (Has (PsErrors :+: State Bang'dExprs) sig m, Data a) => a -> m a
    evacWithNewDo e = maybe (gmapM evacWithNewDo $ e) pure =<< runMaybeT (tryInsertDo e)

    -- surround the expression with a `do` if necessary
    -- We use MaybeT since it has the MonadFail instance we want, as opposed to
    -- the other handlers for 'Empty'
    tryInsertDo :: forall a sig m . (Has (PsErrors :+: State Bang'dExprs) sig m, Data a) => a -> MaybeT m a
    tryInsertDo expr = do
      Refl <- hoistMaybe (eqT @a @LExpr)
      (fromDList -> stmts, expr') <- runWriter (evac expr)
      case nonEmpty stmts of
        Nothing -> pure expr'
        Just neStmts ->
          -- TODO [case stmt] if we want to not touch case expressions that are statements, we need to run evac on this (located!) BodyStmt
          --   ...Except I'm not sure that makes sense because we're running evac on the expression to find out whether we have to turn it into a statement at all (and I certainly don't want two passes)
          --   I think it's best if we just accept that this will have separate binds. You could tell case via Reader that it shouldn't do it here or something, but then it would depend on whether or
          --   not the case itself has bands... It gets complicated fast.
          --   Oh! But I think if we use ! in the case we can automatically rely on it being inside a BodyStmt (or at least of a monadic type), so we can always do the simpler thing if we start here
          let lastStmt = BodyStmt noExtField expr' noExtField noExtField
              doStmts = (fromBindStmt <$> toList neStmts) ++ [noLocA lastStmt]
           in pure . noLocA $
                HsDo EpAnnNotUsed (DoExpr Nothing) (noLocA doStmts)

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

    tryLExpr :: forall a sig m . (Has Fill sig m, Data a) => a -> MaybeT m a
    tryLExpr e = do
      Refl <- hoistMaybe (eqT @a @LExpr)
      ExprLoc loc expr <- pure e
      L l _ <- pure e
      case expr of
        -- Replace holes resulting from `!`
        HsUnboundVar _ _ -> do
          lexpr' <- evac =<< popError loc
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
--         --   localBinds' <- evacWithNewDo grhssLocalBinds
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
      GRHSs exts <$> evac grhss <*> evacWithNewDo localBinds

    -- Find all !s in the given statements and combine the resulting bind
    -- statements into listss, with the original statements being the last one
    -- in each list - then concatenate these lists
    addStmts :: forall sig m . Has (PsErrors :+: State Bang'dExprs) sig m => [ExprLStmt GhcPs] -> m [ExprLStmt GhcPs]
    addStmts = concatMapM \lstmt -> do
      (fromDList -> stmts, lstmt') <- runWriter (evac lstmt)
      pure $ (fromBindStmt <$> stmts) ++ [lstmt']

type DList a = Endo [a]

fromDList :: DList a -> [a]
fromDList = appEndo ?? []

tellOne :: Has (Writer (DList w)) sig m => w -> m ()
tellOne x = tell $ Endo (x:)

type PsErrors = Writer (Messages PsError)

type Fill = PsErrors :+: Writer (DList BindStmt) :+: State Bang'dExprs

type Bang'dExprs = Map Loc LExpr

data BindStmt = RdrName :<- LExpr
              -- let var param1 ... paramn = val
              | Let { var :: RdrName
                    , params :: [RdrName]
                    , val :: LExpr
                    }

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

-- | Look up an error and remove it from the remaining errors if found
-- TODO instead of using State, we could use a custom effect that only supports this
-- TODO or possibly we could just use Reader with local instead, though the callsites would look a little different then
popError :: Has Fill sig m => Loc -> MaybeT m LExpr
popError loc = do
  (merr, remainingErrs) <- M.updateLookupWithKey (\_ _ -> Nothing) loc <$> get
  put remainingErrs
  hoistMaybe merr

-- Use the !'d expression if it's short enough, or else just <!expr>
-- We don't need to worry about shadowing, since we add the line and column numbers
-- TODO Also should we instad use the first n characters followed by ... if it's too long? Seems better
bangVar :: LExpr -> Loc -> RdrName
bangVar (L _ expr) = case lines (showPprUnsafe expr) of
  [str] | length str < 20 -> locVar $ "!" ++ str
  _ -> locVar "<!expr>"

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

hoistMaybe :: Applicative m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure

-- tellPsError :: Has PsErrors sig m => PsError -> SrcSpan -> m ()
-- tellPsError err srcSpan = tell . singleMessage $ MsgEnvelope srcSpan neverQualify err SevError

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
