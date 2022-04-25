{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE PatternSynonyms #-}

module MonadicBang (plugin) where

import Control.Arrow
import Control.Applicative
import Control.Monad.Trans.Maybe
import Control.Carrier.Writer.Strict
import Control.Carrier.State.Strict
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Effect.Reader.Labelled qualified as L
import Control.Effect.Labelled hiding (L)
-- import Control.Effect.Sum hiding (L)
-- import Data.Bifunctor
import Data.Data
import Data.Foldable
-- import Data.Function
import Data.List.NonEmpty (nonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Monoid
import GHC
import GHC.Builtin.Names
import GHC.Data.Bag
import GHC.Iface.Env
import GHC.Hs.Syn.Type
import GHC.Parser.Errors.Types
import GHC.Plugins hiding (Expr, Let, empty, DefaultingPlugin)
import GHC.Rename.Expr
import GHC.Tc.Gen.Expr
import GHC.Tc.Types
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.TcType
-- import GHC.Types.Error
import GHC.Types.Error
-- import GHC.Utils.Monad (concatMapM)
-- import Text.Printf

import Debug.Trace
-- import GHC.Hs.Dump

-- TODO: Write user manual as haddock comment

plugin :: Plugin
plugin = defaultPlugin
  { parsedResultAction = replaceBangs
  , typeCheckResultAction = handleBangs
  , pluginRecompile = purePlugin
  }

-- Okay I'll be honest this is probably not worth doing
-- Usually you will have type signatures, or you can just use a type app like "pure @IO ..."
-- -- We need to have some kind of temporary default Applicative when it's not clear before
-- -- inserting the bind statements for cases like
-- -- pure (!getLine ++ "!")
-- defaultToIdentity :: [CommandLineOption] -> Maybe DefaultingPlugin
-- defaultToIdentity _ = Just DefaultingPlugin
--   { dePluginInit = pure ()
--   , dePluginRun = \_ (ctsElts . approximateWC False -> wanted) -> do pure []
--     -- general idea: get all the tyvars with tyCoVarsOfCtList, check if there's an Applicative constraint involving it, and suggest a custom type BangDefaultApplicative (defined in this module?)
--     -- Then, in our own typechecking pass, these either get unified with the actual Applicative, or if there are any left over, we re-throw the ambiguous variable error
--     -- TODO we could also provide a class/type family that allows users to provide their own defaults for things other than Applicative, so they can properly use qualifiedDo and such
--     -- But this isn't very important since you can always just add a type signature
--   , dePluginStop = const $ pure ()
--   }

-- We don't care about which file things are from, because the entire AST comes
-- from the same module
data Loc = MkLoc {line :: Int, col :: Int}
         deriving (Eq, Ord, Show)

-- type Expr = HsExpr GhcPs
-- type LExpr = LHsExpr GhcPs

-- | Decrement column by one to get the location of a bang
addBang :: Loc -> Loc
addBang loc = loc{col = loc.col - 1}

-- | Used to extract the Loc of a located expression
pattern ExprLoc :: Loc -> HsExpr GhcPs -> LHsExpr GhcPs
pattern ExprLoc loc expr <- L (locA -> RealSrcSpan (spanToLoc -> loc) _) expr

spanToLoc :: RealSrcSpan -> Loc
spanToLoc = liftA2 MkLoc srcLocLine srcLocCol . realSrcSpanStart

replaceBangs :: [CommandLineOption] -> ModSummary -> ParsedResult -> Hsc ParsedResult
replaceBangs _ _ (ParsedResult (HsParsedModule mod' files) msgs) =
  -- trace (showSDocUnsafe $ showAstData BlankSrcSpan BlankEpAnnotations mod') $ -- XXX JB
  -- TODO: have a second module MonadicBangDebug, that does the same thing but also pretty prints the modified AST
  --   Or, since we receive command line options, can we do this via a custom command line option? Looks like yes!
  pure $ ParsedResult (HsParsedModule (let m' = addBangDef $ fillHoles fills mod' in trace (showSDocUnsafe $ ppr m') m') files) msgs{psErrors}
  where
    -- Take out the errors we care about, throw the rest back in
    (mkMessages -> psErrors, M.fromList . bagToList -> fills) =
      (partitionBagWith ?? msgs.psErrors.getMessages) \cases
        err | PsErrBangPatWithoutSpace lexpr@(ExprLoc (addBang -> loc) _) <- err.errMsgDiagnostic
            -> Right (loc, lexpr)
            | otherwise -> Left err

addBangDef :: Located HsModule -> Located HsModule
addBangDef = fmap \mod' -> mod'{hsmodDecls = bangDef ++ mod'.hsmodDecls}
  where
    bangDef :: [LHsDecl GhcPs]
    bangDef =
      [ tySig $ HsAppTy noExtField (var "f") (var "a") --> var "a"
      , valDef lbangName $ HsVar noExtField lbangName
      ]

    a --> b = HsFunTy EpAnnNotUsed (HsExplicitMult noHsTok (var "m") noHsUniTok) (noLocA a) b
    var = noLocA . HsTyVar EpAnnNotUsed NotPromoted . noLocA . mkUnqual tvName . fsLit

    tySig = noLocA . SigD noExtField . TypeSig EpAnnNotUsed [lbangName] . HsWC noExtField . noLocA . HsSig noExtField (HsOuterImplicit noExtField) . noLocA
    valDef name body = noLocA . ValD noExtField $ FunBind noExtField name (mg name body) []
    mg name body = (MG noExtField (noLocA [noLocA $ Match EpAnnNotUsed (FunRhs name GHC.Prefix NoSrcStrict) [] (grhss body)]) Generated)
    grhss body = GRHSs emptyComments [noLocA . GRHS EpAnnNotUsed [] $ noLocA body] emptyLocalBinds

lbangName :: LIdP GhcPs
lbangName = noLocA bangName

bangName :: RdrName
-- Using identifier followed by backspaces to retain apparent `!` name while
-- pretty-printing, but avoid clashes with potential operators named `!`
-- TODO check if we can instead use an exact name without clashing
bangName = mkVarUnqual $ fsLit "m\bb\b!"

-- | Replace holes in an AST whenever an expression with the corresponding
-- source span can be found in the given list.
fillHoles :: Data a => BangErrors -> a -> a
fillHoles fillers ast = case run $ runState fillers (insertExprs ast) of
  (remainingErrs, ast') -> case nonEmpty (toList remainingErrs) of
    Nothing -> ast'
    Just _neErrs -> error "Found extraneous bangs" -- TODO improve error msg (incl. bug report url)
                                                   -- Use PsUnknownMessage? (maybe not since this is a panic)
  where
    insertExprs :: forall a sig m . (Has (State BangErrors) sig m, Data a) => a -> m a
    insertExprs e = fromMaybeM (gmapM insertExprs e) =<< runMaybeT (tryLExpr e)

    -- We use MaybeT here instead of fused-effects' 'Empty' because it has the
    -- MonadFail instance we want
    tryLExpr :: forall a sig m . (Has (State BangErrors) sig m, Data a) => a -> MaybeT m a
    tryLExpr e = do
      Refl <- hoistMaybe (eqT @a @(LHsExpr GhcPs))
      ExprLoc loc (HsUnboundVar _ _) <- pure e
      -- Replace holes resulting from `!`
      fmap insertBang . insertExprs =<< popError loc

    insertBang :: LHsExpr GhcPs -> LHsExpr GhcPs
    insertBang lexpr = L includePrevCol . HsApp EpAnnNotUsed (L prevColSpan . HsVar noExtField $ L prevColSpan bangName) $ lexpr
      where
        srcSpan :: SrcSpan
        srcSpan = (getLoc lexpr).locA

        prevCol :: SrcSpan
        prevCol = case srcSpanStart srcSpan of
          UnhelpfulLoc _ -> srcSpan
          RealSrcLoc start _ -> srcLocSpan $
            mkSrcLoc (srcLocFile start) (srcLocLine start) (srcLocCol start - 1)

        prevColSpan :: SrcAnn a
        prevColSpan = SrcSpanAnn EpAnnNotUsed prevCol

        includePrevCol :: SrcSpanAnnA
        includePrevCol = SrcSpanAnn EpAnnNotUsed $ combineSrcSpans srcSpan prevCol

-- temporary bottom value used to fill in tree branches that are later replaced
-- by the actual expression
-- TODO make sure this also works when Prelude isn't imported
mkTmpBottom :: TcM (LHsExpr GhcPs)
mkTmpBottom = noLocA . HsVar noExtField . noLocA . getRdrName <$> lookupOrig pRELUDE (mkVarOcc "undefined")

-- TODO: have a multi-module test to ensure ! definitions don't clash
handleBangs :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
handleBangs _ _ gblEnv = do
  tcg_binds' <- traverse createDos $ removeBangDef gblEnv.tcg_binds
  liftIO . traverse_ (\x -> putStrLn "BIND:" *> putStrLn (showPprUnsafe x)) $ toList tcg_binds' -- XXX JB
  pure gblEnv{tcg_binds = tcg_binds'}
  where
    -- We don't need the ! function anymore after typechecking
    removeBangDef :: LHsBinds GhcTc -> LHsBinds GhcTc
    removeBangDef = filterBag $ unLoc >>> trace "casing" \cases
      (XHsBindsLR AbsBinds{abs_binds = toList -> [L _ FunBind{fun_id}]}) -> occName (unLoc fun_id) /= occName bangName
      _ -> True

-- | create 'HsDo's where necessary or add statements to existing ones
-- While it could be nice to always add new 'HsDo's rather than having to take
-- into account existing ones, we cannot do that, since we need to use the
-- existing 'HsDo' in the case of e.g. bangs being used in the guard of a list
-- comprehension.
-- TODO Approach: Traverse, and when inserting do/adding stmts to a do, construct a GhcPs tree with `let x = x in x` (fresh name though so it doesn't shadow things)
-- Then, unify the inferred type of x with the actual type of the wrapped expr, and unify the type of the new do with the type of the surrounding expr
-- But, how do we deal with existing Dos? Since we create the Dos as GhcPs but the existing ones are GhcTc, I'm not sure.
-- You would think that maybe we could just rename and typecheck the statements to be inserted separately, without looking at the do block as a whole - and maybe that's true.
-- The one concern (apart from minor ones, like, do we have to treat the last statement specially somehow?) I have is ApplicativeDo.
-- The renamer creates an ApplicativeDo statement, what if the statements we insert means it can't be Applicative anymore?
-- I suspect we could in principle detect that manually, and revert the ApplicativeDo statement back to a regular do if necessary,
-- but it gets complicated when you consider that you can enable different algorithms for ApplicativDo.
-- So a better approach might be to somehow turn the do block back into a GhcPs state.
-- One option would be to find the appropriate tree branch in the ParsedModule, but I'm afraid of renaming/typechecking messages showing up multiple times then.
-- It's not easy...
-- Also, if it does turn out that ApplicativeDo doesn't work, do we have to bubble up the new Monad constraint? I'm guessing yes... Which doesn't sound fun :/
-- One possibility: Create a skeleton of the previous HsDo, replacing expressions with variables that are added to the environment,
-- and which have the typechecked types assigned to them, and add the new statements to that skeleton. At the end, replace the variables again with the proper expressions.
-- This could work, but of course doesn't solve all problems, like the bubbling
-- up, which seems like a real Menace. I'm thinking we might just give up on
-- type inference there and say, if you haven't declared a Monad constraint
-- where you need one, you're just gonna have to add one.
-- The reason I'm saying this is because typechecking is global: If we add an inferred Monad constraint here, that could affect binds all over the module. Better to just keep it local and have this small infelicity.
-- The good news is that we can have a nice custom error message explaining this.
--
-- ApplicativeDo has some other potential issues though:
-- let's say we have some statements, and the renamer makes an ApplicativeDo stmt.
-- We can recover the original HsDo, but since the expressions are already
-- renamed, we can't run them through the renamer again to find out whether we
-- can use Applicative.
-- And if we replace every rhs of bind statements with a single tmpBottom
-- variable, it's going to look like they don't depend on anything, which would
-- mean that ApplicativeDo would trigger on newly such constructed HsDos, even
-- if they do depend on other things.
-- Ideally, we would build a regular HsDo skeleton, but somehow keep track of any dependencies (by including them in the rhs, as arguments to tmpBottom?)
-- Since ApplicativeStmts are constructed by taking into account the
-- dependencies, it seems as though it ought to be possible to recover the
-- dependencies from them, or at least an equivalent set of dependencies.
createDos :: LHsBind GhcTc -> TcM (LHsBind GhcTc)
createDos bind = do
  tmpBottom <- mkTmpBottom
  runM . runReader tmpBottom . runLabelled @"tmpBottom" $ createDo bind
  where
    createDo :: (Has (Lift TcM) sig m, HasTmpBottom sig m, Data a) => a -> m a
    createDo e = fromMaybeM (gmapM createDo e) =<< runMaybeT (tryInsertDo e)

    tryInsertDo :: forall a sig m . (Has (Lift TcM) sig m, HasTmpBottom sig m, Data a) => a -> MaybeT m a
    tryInsertDo expr = do
      Refl <- hoistMaybe $ eqT @a @(LHsExpr GhcTc)
      (fromDList -> stmts, expr') <- runWriter (evac expr)
      case nonEmpty stmts of
        Nothing -> pure expr'
        Just neStmts -> do
          tmpBottom <- L.ask @"tmpBottom"
          let lastStmt = BodyStmt noExtField tmpBottom noExtField noExtField
              doStmts = (fromBindStmt <$> toList neStmts) ++ [noLocA lastStmt]
              newDo = HsDo EpAnnNotUsed (DoExpr Nothing) (noLocA doStmts)
          -- TODO do we need to do something with the FreeVars?
          (renamedDo, _) <- sendM $ rnExpr newDo
          noLocA <$> sendM (tcExpr renamedDo (Check $ lhsExprType expr'))

    evac :: forall a sig m . (Has (Writer (DList BindStmt) :+: Lift TcM) sig m, HasTmpBottom sig m, Data a) => a -> m a
    evac e = maybe (gmapM evac $ e) pure =<< runMaybeT (asum $ [tryLExpr] ?? e)

    tryLExpr :: forall a sig m . (Has (Writer (DList BindStmt) :+: Lift TcM) sig m, HasTmpBottom sig m, Data a) => a -> MaybeT m a
    tryLExpr e = do
      Refl <- hoistMaybe $ eqT @a @(LHsExpr GhcTc)
      L l expr <- pure e
      case expr of
        HsApp _ (unLoc -> XExpr (WrapExpr (HsWrap _ (HsVar _ (L _ name))))) _
          | occName name == occName bangName -> do
              name <- getRdrName <$> sendM (newNameAt (bangVar expr) l.locA)
              e' <- evac e
              tellOne (name :<- e')
              pure e'
        _ -> empty

-- createDos :: Data a => BangErrors -> a -> a
-- createDos fillers ast = case run $ runState fillers (evacWithNewDo ast) of
--   (remainingErrs, ast') -> case nonEmpty (toList remainingErrs) of
--     Nothing -> ast'
--     Just _neErrs -> error "Found extraneous bangs" -- TODO improve error msg (incl. bug report url)
--                                                    -- Use PsUnknownMessage? (maybe not since this is a panic)
--   where
--     evacWithNewDo :: forall a sig m . (Has (State BangErrors) sig m, Data a) => a -> m a
--     evacWithNewDo e = maybe (gmapM evacWithNewDo $ e) pure =<< runMaybeT (tryInsertDo e)

--     -- surround the expression with a `do` if necessary
--     -- We use MaybeT since it has the MonadFail instance we want, as opposed to
--     -- the other handlers for 'Empty'
--     tryInsertDo :: forall a sig m . (Has (State BangErrors) sig m, Data a) => a -> MaybeT m a
--     tryInsertDo expr = do
--       Refl <- hoistMaybe (eqT @a @LExpr)
--       (fromDList -> stmts, expr') <- runWriter (evac expr)
--       case nonEmpty stmts of
--         Nothing -> pure expr'
--         Just neStmts ->
--           -- TODO [case stmt] if we want to not touch case expressions that are statements, we need to run evac on this (located!) BodyStmt
--           --   ...Except I'm not sure that makes sense because we're running evac on the expression to find out whether we have to turn it into a statement at all (and I certainly don't want two passes)
--           --   I think it's best if we just accept that this will have separate binds. You could tell case via Reader that it shouldn't do it here or something, but then it would depend on whether or
--           --   not the case itself has bands... It gets complicated fast.
--           --   Oh! But I think if we use ! in the case we can automatically rely on it being inside a BodyStmt (or at least of a monadic type), so we can always do the simpler thing if we start here
--           let lastStmt = BodyStmt noExtField expr' noExtField noExtField
--               doStmts = (fromBindStmt <$> toList neStmts) ++ [noLocA lastStmt]
--            in pure . noLocA $
--                 HsDo EpAnnNotUsed (DoExpr Nothing) (noLocA doStmts)

--     evac :: forall a sig m . (Has Fill sig m, Data a) => a -> m a
--     evac e = maybe (gmapM evac $ e) pure =<< runMaybeT (asum $ [tryLExpr, tryStmt, tryGRHSs] ?? e)

--     tryStmt :: forall a sig m . (Has Fill sig m, Data a) => a -> MaybeT m a
--     tryStmt e = do
--       Refl <- hoistMaybe (eqT @a @(ExprStmt GhcPs))
--       case e of
--         RecStmt{recS_stmts} -> do
--           recS_stmts' <- traverse addStmts recS_stmts
--           pure e{recS_stmts = recS_stmts'}
--         ParStmt xp stmtBlocks zipper bind -> do
--           stmtsBlocks' <- traverse addParStmts stmtBlocks
--           pure $ ParStmt xp stmtsBlocks' zipper bind
--           where
--             addParStmts :: ParStmtBlock GhcPs GhcPs -> MaybeT m (ParStmtBlock GhcPs GhcPs)
--             addParStmts (ParStmtBlock xb stmts vars ret) = do
--               stmts' <- addStmts stmts
--               pure $ ParStmtBlock xb stmts' vars ret
--         _ -> empty

--     tryLExpr :: forall a sig m . (Has Fill sig m, Data a) => a -> MaybeT m a
--     tryLExpr e = do
--       Refl <- hoistMaybe (eqT @a @LExpr)
--       ExprLoc loc expr <- pure e
--       L l _ <- pure e
--       case expr of
--         -- Replace holes resulting from `!`
--         HsUnboundVar _ _ -> do
--           lexpr' <- evac =<< popError loc
--           let name = bangVar lexpr' loc
--           tellOne (name :<- lexpr')
--           evac . L l $  HsVar noExtField (noLocA name)
--         -- For case, we only bind the actions in the alternative that is
--         -- actually chosen - as well as any actions used in view patterns or
--         -- guards for that or previous alternatives
--         -- TODO: The HsDo constructed by this should take into account whether or not we're inside a qualifiedDo
--         --       by putting the module name inside reader
--         -- TODO: ideally we only want to transform HsCase when it's actually necessary
--         --       by writing to a different writer (Any newtype) that records whether or not we have to
--         -- TODO: If this case expression is a statement in a do block, we don't need to do any transformations just the idris treatment is enough
--         --       However, the same is true if it's just preceded by "id" or more generally has a monadic type, so we can't handle this consistently anyway, so maybe we just always want to treat it the same?
--         --       Especially when you keep in mind that even inside do blocks, you can actually have non-monadic stuff (e.g. "do case () of () -> 4") EXCEPT! That's not true anymore as soon as you use ! inside the do
--         --       see TODO [case stmt] for more questionable circumstances
--         -- TODO: Technically, you could also split up nested patterns, such that in (Just (!foo -> X)), !foo is only executed in the Just case
--         --       seems doable
--         -- TODO: we might get unused warnings with the scrutinee if we're not careful. I guess we'll have to traverse the tree to see if the scrutinee is being used...?
--         -- -- TODO: we might get unused warnings with the scrutinee if we're not careful. I guess we'll have to traverse the tree to see if the scrutinee is being used...?
--         -- HsCase xc scrut mg -> do
--         --   scrut' <- evac scrut
--         --   (lambdas, mg') <- evacMatchGroup mg
--         --   let binds = locVar "binds for case" loc
--         --       bindsExpr = L l $ HsCase xc scrut' mg'
--         --       bindsVar = noLocA . HsVar noExtField $ noLocA binds
--         --   tellOne (binds :<- bindsExpr)
--         --   pure $ foldl' ((noLocA .) . HsApp EpAnnNotUsed) bindsVar lambdas
--         -- If we encounter a `do`, use that instead of a `do` we inserted
--         HsDo xd ctxt stmts -> L l . HsDo xd ctxt <$> traverse addStmts stmts
--         _ -> empty

-- --     evacMatchGroup :: Has Fill sig m => MatchGroup GhcPs LExpr -> m ([LExpr], MatchGroup GhcPs LExpr)
-- --     evacMatchGroup mg@MG{mg_alts} = do
-- --       let L l alts = mg_alts
-- --       (binds, alts') <- evacAlts alts
-- --       traverse_ tellOne binds
-- --       pure ([], mg{mg_alts = L l alts'})
-- --       where
-- --         -- Returns any binds from the first alternative as well as the modified matches
-- --         evacAlts :: forall sig m . Has (State BangErrors) sig m => [LMatch GhcPs LExpr] -> m ([BindStmt], [LMatch GhcPs LExpr])
-- --         evacAlts (lmatch : lmatches) = do
-- --           (nonEmpty . fromDList -> firstAltBinds, pats') <- runWriter $ evacAlt lmatch
-- --           error "TODO~"
-- --         -- evacAlts [] = pure ([], [])
-- --         -- evacAlts (L l match@Match{m_pats, m_ctxt, m_grhss} : lmatches) = do
-- --         --   (fromDList -> firstAltBinds, pats') <- runWriter $ evac m_pats
-- --         --   localBinds' <- evacWithNewDo grhssLocalBinds
-- --         --   (binds, lmatches') <- evacAlts lmatches
-- --         --   evacLGRHSs grhssGRHSs >>= \cases
-- --         --     (Right grhss') -> do
-- --         --       let lmatch' = L l match{m_pats = pats', m_grhss = m_grhss{grhssGRHSs = grhss', grhssLocalBinds = localBinds'}}
-- --         --       case nonEmpty binds of -- TODO even though we need a regular list this is actually still safer if we use nonEmpty and toList
-- --         --         Nothing -> pure (firstAltBinds, lmatch' : lmatches')
-- --         --         Just neBinds -> do
-- --         --           -- Example of what's happenning here:
-- --         --           --   case s of
-- --         --           --     a -> ...
-- --         --           --     (!b -> X) -> ...
-- --         --           --     c -> ...
-- --         --           -- becomes
-- --         --           --   case s of
-- --         --           --     a -> ...
-- --         --           --     <scrutinee> -> do
-- --         --           --       <!b> <- b
-- --         --           --       case <scrutinee> of
-- --         --           --         (<!b> -> X) -> ...
-- --         --           --         c -> ...
-- --         --           scrutVar <- newScrutVar
-- --         --           let newMG = MG noExtField (noLocA lmatches') Generated
-- --         --               newCase = HsCase EpAnnNotUsed (noLocA $ HsVar noExtField $ noLocA scrutVar) newMG
-- --         --               newStmt = BodyStmt noExtField (noLocA newCase) noExtField noExtField
-- --         --               newExpr = HsDo EpAnnNotUsed (DoExpr Nothing) (noLocA $ (fromBindStmt <$> toList neBinds) ++ [noLocA newStmt])
-- --         --               newGRHS = GRHS EpAnnNotUsed [] (noLocA newExpr)
-- --         --               newGRHSs = GRHSs emptyComments [noLocA newGRHS] emptyLocalBinds
-- --         --               newMatch = noLocA $ Match EpAnnNotUsed m_ctxt [noLocA . VarPat noExtField $ noLocA scrutVar] newGRHSs
-- --         --           pure (firstAltBinds, [lmatch', newMatch])
-- --         --     (Left _) -> error "TODO Left"

-- --         evacAlt :: forall sig m . Has (State BangErrors) sig m => LMatch GhcPs LExpr -> m ([BindStmt], [LMatch GhcPs LExpr])
-- --         evacAlt (L l Match{m_pats, m_ctxt, m_grhss}) = do
-- --           error "TODO alt"
-- --           -- (nonEmpty . fromDList -> binds, remainingPats) <- runWriter $ evacPat pat

-- --         evacPat :: forall sig m . Has Fill sig m => LPat GhcPs -> m (Either (LPat GhcPs, [([BindStmt], LPat GhcPs, RdrName)]) (LPat GhcPs))
-- --         evacPat lpat@(L l pat) = case pat of
-- --           (WildPat _) -> pure $ Right lpat
-- --           (VarPat _ _) -> pure $ Right lpat
-- --           (LazyPat xp p) -> L l . LazyPat xp <$$$> evacPat p
-- --           (AsPat xp i p) -> L l . AsPat xp i <$$$> evacPat p
-- --           (ParPat xp lt p rt) -> L l . (ParPat xp lt ?? rt) <$$$> evacPat p
-- --           (BangPat xp p) -> L l . BangPat xp <$$$> evacPat p
-- --           (ListPat xp ps) -> do
-- --             (ps', withBangs) <- traverseUntilLeft evacPat ps
-- --             case withBangs of
-- --               Nothing -> pure $ Right lpat
-- --               -- Oh boy I don't even know. I don't think checking rest is what we have to do here, we have to check whether there's any patterns that were produced by the first pat that contain bangs, or if rest contains bangs
-- --               -- feel like there's a better approach here but ugh I don't know what it is
-- --               Just (firstBang, rest) -> case nonEmpty rest of
-- --           where
-- --             infixl 4 <$$$>
-- --             (<$$$>) = (fmap . first . first)
-- --           --   (Left _) -> error "TODO Left"

-- --         -- If there is at least one guard that requires bind statements, it it
-- --         -- returns those, and the modified GRHSs up to that point, and the
-- --         -- guards up to that point, and the unmodified GRHS without that guard,
-- --         -- and the remaining unmodified GRHSs
-- --         -- Otherwise returns the modified GRHSs
-- --         evacLGRHSs :: forall sig m . Has (State BangErrors) sig m => [LGRHS GhcPs LExpr] ->
-- --           m (Either ([BindStmt], [LGRHS GhcPs LExpr], [GuardLStmt GhcPs], LGRHS GhcPs LExpr, [LGRHS GhcPs LExpr]) [LGRHS GhcPs LExpr])
-- --         evacLGRHSs grhss = do
-- --           (grhss', rest) <- untilLeft evacLGRHS grhss
-- --           case rest of
-- --             Nothing -> pure $ Right grhss'
-- --             Just ((stmts, guards, grhs), grhssRest) -> pure $ Left (stmts, grhss', guards, grhs, grhssRest)

-- --         -- If there is at least one guard that requires bind statements, it
-- --         -- returns those, and the guards up to that point, and the unmodified
-- --         -- GRHS without those guards
-- --         -- Otherwise returns the modified GRHS
-- --         evacLGRHS :: forall sig m . Has (State BangErrors) sig m => LGRHS GhcPs LExpr ->
-- --           m (Either ([BindStmt], [GuardLStmt GhcPs], LGRHS GhcPs LExpr) (LGRHS GhcPs LExpr))
-- --         evacLGRHS lgrhs@(L l (GRHS xg guards body)) = case guards of
-- --           [] -> pure $ Right lgrhs -- TODO replace holes and make sum type and what not
-- --           g:gs -> do
-- --             (fromDList -> stmts, g') <- runWriter $ evac g
-- --             case nonEmpty stmts of
-- --               Nothing -> addGuard g' <$> evacLGRHS (L l $ GRHS xg gs body)
-- --               Just neStmts -> pure $ Left (toList neStmts, [g'], L l $ GRHS xg gs body)
-- --           where
-- --             addGuard g = \cases
-- --               (Left (stmts, gs, rhs)) -> Left (stmts, g:gs, rhs)
-- --               (Right (L l' (GRHS @GhcPs xg' gs body'))) -> Right (L l' $ GRHS xg' (g:gs) body')

-- --         untilLeft :: forall m a b e . Monad m => (a -> m (Either e b)) -> [a] -> m ([b], Maybe (e, [a]))
-- --         untilLeft f = (first reverse <$>) . go []
-- --           where
-- --             go acc [] = pure (acc, Nothing)
-- --             go acc (x:xs) = f x >>= \cases
-- --               (Left e) -> pure (acc, Just (e, xs))
-- --               (Right r) -> go (r:acc) xs

--     -- This lets us start new do-blocks in where blocks
--     tryGRHSs :: forall a sig m . (Has Fill sig m, Data a) => a -> MaybeT m a
--     tryGRHSs e = do
--       Refl <- hoistMaybe (eqT @a @(GRHSs GhcPs LExpr))
--       GRHSs exts grhss localBinds <- pure e
--       GRHSs exts <$> evac grhss <*> evacWithNewDo localBinds

--     -- Find all !s in the given statements and combine the resulting bind
--     -- statements into listss, with the original statements being the last one
--     -- in each list - then concatenate these lists
--     addStmts :: forall sig m . Has (State BangErrors) sig m => [ExprLStmt GhcPs] -> m [ExprLStmt GhcPs]
--     addStmts = concatMapM \lstmt -> do
--       (fromDList -> stmts, lstmt') <- runWriter (evac lstmt)
--       pure $ (fromBindStmt <$> stmts) ++ [lstmt']

type DList a = Endo [a]

fromDList :: DList a -> [a]
fromDList = appEndo ?? []

tellOne :: Has (Writer (DList w)) sig m => w -> m ()
tellOne x = tell $ Endo (x:)

-- type Fill = Writer (DList BindStmt) :+: State BangErrors

type HasTmpBottom sig m = HasLabelled "tmpBottom" (Reader (LHsExpr GhcPs)) sig m

type BangErrors = Map Loc (LHsExpr GhcPs)

data BindStmt = RdrName :<- LHsExpr GhcTc
              -- | Let { var :: RdrName
              --       , params :: [RdrName]
              --       , val :: LHsExpr GhcPs
              --       } -- ^ let var param1 ... paramn = val

fromBindStmt :: BindStmt -> ExprLStmt GhcPs
fromBindStmt = noLocA . \cases
  -- TODO: each bind statement needs to get dummy tmpBottom, which will later be replaced (after typechecking and renaming.)
  (var :<- lexpr) -> BindStmt EpAnnNotUsed varPat lexpr
    where
      varPat = noLocA . VarPat noExtField $ noLocA var
  -- Let{var, params, val} -> LetStmt EpAnnNotUsed $ binding
  --   where
  --     lvar = noLocA var
  --     binding = HsValBinds EpAnnNotUsed valBinds
  --     valBinds = ValBinds NoAnnSortKey (unitBag . noLocA $ FunBind noExtField lvar mg []) []
  --     mg = MG noExtField (noLocA [noLocA match]) Generated
  --     pats = noLocA . VarPat noExtField . noLocA <$> params
  --     match = Match EpAnnNotUsed (FunRhs lvar GHC.Prefix NoSrcStrict) pats . GRHSs emptyComments [rhs] $
  --       EmptyLocalBinds noExtField
  --     rhs = noLocA $ GRHS EpAnnNotUsed [] val

-- | Look up an error and remove it from the remaining errors if found
popError :: Has (State BangErrors) sig m => Loc -> MaybeT m (LHsExpr GhcPs)
popError loc = do
  (merr, remainingErrs) <- M.updateLookupWithKey (\_ _ -> Nothing) loc <$> get
  put remainingErrs
  hoistMaybe merr

-- Use the !'d expression if it's short enough, or else just <!expr>
bangVar :: HsExpr GhcTc -> OccName
bangVar expr = mkVarOcc case lines (showPprUnsafe expr) of
  [str] | length str < 20 -> "!" ++ str
  _ -> "<!expr>"

-- locVar :: String -> Loc -> OccName
-- -- using spaces and special characters should make it impossible to overlap
-- -- with user-defined names (but could still technically overlap with names
-- -- introduced by other plugins)
-- locVar str loc = mkVarUnqual . fsLit $
--   printf "<%s:%d:%d>" str loc.line loc.col

-- -- TODO use Fresh to avoid shadowing
-- -- actually, Fresh is not necessary if we use newName, but might still be nice when pretty printing (though pretty printing includes the unique typically, so maybe doesn't matter?)
-- newScrutVar :: Monad m => m RdrName
-- newScrutVar = pure . mkVarUnqual . fsLit $ "<scrutinee>"

hoistMaybe :: Applicative m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure

-- -- traverseUntilLeft :: Monad f => (a -> f (Either b c)) -> [a] -> f ([c], Maybe (b, [a]))
-- -- traverseUntilLeft f = fix \go -> \cases
-- --   [] -> pure ([], Nothing)
-- --   (x:xs) -> f x >>= \cases
-- --     (Left b) -> pure ([], Just (b, xs))
-- --     (Right c) -> first (c:) <$> go xs

-- -- traverseToFst :: Functor f => (a -> f b) -> a -> f (b, a)
-- -- traverseToFst f x = (, x) <$> f x

(??) :: Functor f => f (a -> b) -> a -> f b
fs ?? x = ($ x) <$> fs

fromMaybeM :: Monad m => m a -> Maybe a -> m a
fromMaybeM = maybe ?? pure
