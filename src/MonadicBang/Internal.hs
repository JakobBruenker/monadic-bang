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
import Control.Exception hiding (try)
import Data.Data
import Data.Foldable
import Data.Functor
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Monoid
import GHC
import GHC.Data.Bag
import GHC.Data.Maybe
import GHC.Parser.Errors.Types
import GHC.Plugins hiding (Expr, empty, (<>), panic, try)
import GHC.Types.Error
import GHC.Utils.Monad (concatMapM, whenM)
import Text.Printf

import Debug.Trace

import GHC.Utils.Logger

import MonadicBang.Effect.Offer
import MonadicBang.Effect.Uniques
import MonadicBang.Options
import MonadicBang.Utils
import MonadicBang.Error

-- TODO: do we want to add Reader DynFlags with showPpr instead of using showPprUnsafe?

-- We don't care about which file things are from, because the entire AST comes
-- from the same module
data Loc = MkLoc {line :: Int, col :: Int}
         deriving (Eq, Ord, Show)

type Expr = HsExpr GhcPs
type LExpr = LHsExpr GhcPs

-- | To keep track of which local variables in scope may be used
--
-- If local variables are defined within the same do block as a !, but outside
-- of a !, they must not be used, since their desugaring would make them escape
-- their scope.
data InScope = MkInScope {valid :: OccSet , invalid :: OccSet}

instance Semigroup InScope where
  a <> b = MkInScope{valid = a.valid <> b.valid, invalid = a.invalid <> b.invalid}

instance Monoid InScope where
  mempty = noneInScope

noneInScope :: InScope
noneInScope = MkInScope emptyOccSet emptyOccSet

addValid :: OccName -> InScope -> InScope
addValid name inScope = inScope{valid = extendOccSet inScope.valid name}

addValids :: OccSet -> InScope -> InScope
addValids names inScope = inScope{valid = inScope.valid <> names}

invalidateVars :: InScope -> InScope
invalidateVars inScope = MkInScope{valid = emptyOccSet, invalid = inScope.valid <> inScope.invalid}

isInvalid :: Has (Reader InScope) sig m => OccName -> m Bool
isInvalid name = do
  inScope <- ask @InScope
  pure $ name `elemOccSet` inScope.invalid

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
  -- TODO since the output of the Writer is not used, we should add a carrier providing evalWriter, which just ignores `tell`.
  (newErrors, mod'') <- runM . runUniquesIO 'p' . runWriter . runReader options . runReader noneInScope . fmap snd . runWriter @OccSet $ fillHoles fills mod'
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
    else panic $ "Found extraneous bangs:" ++ unlines (showPprUnsafe <$> toList remainingErrs)
  where
    psError expr = \cases
      Preserve      -> PsErrBangPatWithoutSpace expr
      Don'tPreserve -> customError ErrBangOutsideOfDo

    -- XXX JB seems like we probably only need one evac, not two... not sure though, actually
    evac :: forall a sig m . (Has Fill sig m, Data a) => a -> m a
    -- This recurses over all nodes in the AST, except for nodes for which
    -- one of the `try` functions returns `Just <something>`.
    evac e = maybe (gmapM evac e) pure =<< runMaybeT (tryEvac usualTries e)

    tryEvac :: Monad m => [a -> MaybeT m a] -> a -> MaybeT m a
    tryEvac tries = asum . (tries ??)

    -- TODO: Via benchmarking, find out whether it makes sense to `try` more
    -- datatypes here (e.g. `trySrcSpan`) that would always remain unmodified
    -- anyway due to not containing expressions, and thus don't need to be
    -- recursed over (in that case you could probably have a function like
    -- `ignore @SrcSpan`, which would simplify things)
    -- TODO sort _roughly_ by how often each try will succeed (micro-optimization)
    usualTries :: (Has Fill sig m, Data a) => [a -> MaybeT m a]
    -- XXX JB maybe they should all be handlers, and then we map try over them here (not actually map because that would require a het list)
    usualTries = [tryHsBindLR, try handleMatchGroup, tryLExpr, tryStmtLR]

    -- | We keep track of any local binds, to prevent the user from using them
    -- with ! in situations where they would be evacuated to a place where
    -- they're not in scope
    --
    -- The plugin would still work without this, but might accept programs that
    -- shouldn't be accepted, with unexpected semantics. E.g:
    --
    -- > do let s = pure "outer"
    -- >    let s = pure "inner" in putStrLn !s
    --
    -- You might expect this to print `inner`, but it would actually print
    -- `outer`, since it would be desugared to
    --
    -- > do let s = pure "outer"
    -- >    <!s> <- s
    -- >    let s = pure "inner" in print <!s>
    --
    -- With this function, the plugin will instead throw an error saying that
    -- `s` cannot be used here.
    --
    -- If the first `s` weren't defined, the user would, without this function,
    -- get an error saying that `s` is not in scope, at the call site. Here,
    -- we instead throw a more informative error.
    tryHsBindLR :: forall a sig m . (Has Fill sig m, Data a) => a -> MaybeT m a
    tryHsBindLR = try \(bind :: HsBindLR GhcPs GhcPs) -> case bind of
      FunBind{fun_id = occName . unLoc -> name, fun_matches = matches} -> do
        tellLocalVar name
        fun_matches <-  local (addValid name) $ handleMatchGroup matches
        pure bind{fun_matches}
      PatBind{pat_lhs = lhs, pat_rhs = rhs} -> do
        (binds, pat_lhs) <- ask @InScope >>= flip runState (traverse evacPats lhs)
        pat_rhs <- local (<> binds) $ handleGRHSs rhs
        pure bind{pat_lhs, pat_rhs}
      -- All VarBinds are introduced by the type checker, but we might as well handle them
      VarBind{var_id = occName -> name, var_rhs = expr} -> do
        tellLocalVar name
        var_rhs <- local (addValid name) $ evac expr
        pure bind{var_rhs}
      -- Pattern synonyms can never appear inside of do blocks, so we don't have
      -- to handle them specially
      PatSynBind{} -> empty
    
    -- XXX JB should these be called evac instead of handle? idk though handler isn't bad
    -- XXX JB could have `type Handler m a = a -> m a`
    -- XXX JB we could even make a class Handle that provides "handle" and
    -- XXX JB "try", and make instances for these guys
    handleGRHSs :: forall sig m . Has Fill sig m => GRHSs GhcPs LExpr -> m (GRHSs GhcPs LExpr)
    handleGRHSs grhss = do
      patVars <- ask @InScope
      grhssLocalBinds <- local (<> patVars) $ evac grhss.grhssLocalBinds
      grhssGRHSs <- evalState patVars $ evacPats grhss.grhssGRHSs
      pure grhss{grhssGRHSs, grhssLocalBinds}

    handleMatchGroup :: forall sig m . Has Fill sig m => MatchGroup GhcPs LExpr -> m (MatchGroup GhcPs LExpr)
    handleMatchGroup mg = do
      mg_alts <- (traverse . traverse . traverse) handleMatch mg.mg_alts
      pure mg{mg_alts}
      where
        handleMatch :: Match GhcPs LExpr -> m (Match GhcPs LExpr)
        handleMatch match = do
          -- We use the State to keep track of the bindings that have been
          -- introduced in patterns to the left of the one we're currently looking
          -- at. Example:
          --
          -- > \a (Just [b, (+ b) -> d]) (foldr a b -> c) | Just f <- b, f == 24
          --
          -- the view pattern on `c` has access to the variables to the left of it. The same applies to `d`.
          -- `f == 24` additionally has access to variables defined in the guard to its left.
          (patVars, m_pats) <- ask @InScope >>= runState ?? evacPats match.m_pats
          m_grhss <- local (<> patVars) $ handleGRHSs match.m_grhss
          pure match{m_pats, m_grhss}

    -- evacuate !s in pattern and collect all the names it binds
    evacPats :: forall a m sig . (Has (Fill :+: State InScope) sig m, Data a) => a -> m a
    evacPats e = do
      currentState <- get @InScope
      maybe (gmapM evacPats e) pure =<< runMaybeT (tryEvac ((local (<> currentState) .) <$> (tryPat : usualTries)) e)

    -- XXX JB I think we should replace this by tryRdrName -- XXX JB HOWEVER: binds in do statements should be ignored. Sooo maybe it's safer to go this route after all.
    -- XXX JB HOWEVER no. 2: I think we don't actually need to worry about binds in do statements - since we special case HsDo.
    -- XXX JB the one thing we'd need to special case as well though is `a <- a`, since the bind here isn't visible
    -- XXX JB ...overall I still feel like I'm more comfortable special casing HsBindLR and patterns...
    tryPat :: forall a m sig . (Has (Fill :+: State InScope) sig m, Data a) => a -> MaybeT m a
    tryPat = try \(p :: Pat GhcPs) -> case p of
      VarPat xv name -> tellName name $> VarPat xv name
      AsPat xa name pat -> do
        tellName name
        AsPat xa name <$> traverse (liftMaybeT . evacPats) pat

      _ -> empty
      where
        tellName (occName . unLoc -> name) = do
          tellLocalVar name
          modify $ addValid name

    tryLExpr :: forall a sig m . (Has Fill sig m, Data a) => a -> MaybeT m a
    tryLExpr = try \e@(L l _) -> do
      ExprLoc loc expr <- pure e
      case expr of
        -- Replace holes resulting from `!`
        -- If no corresponding expression can be found in the Offer, we assume
        -- that it was a hole put there by the user and leave it unmodified
        HsUnboundVar _ _ -> yoink loc >>= maybe (pure e) \lexpr -> do
          -- all existing valid local variables now become invalid, since using
          -- them would make them escape their scope
          lexpr' <- local invalidateVars $ evac lexpr
          name <- bangVar lexpr' loc
          tellOne $ name :<- lexpr'
          -- XXX JB pretty sure we don't need the evac here
          -- evac . L l $ HsVar noExtField (noLocA name)
          pure . L l $ HsVar noExtField (noLocA name)
        HsVar _ (occName . unLoc -> name) -> do
          whenM (asks @InScope $ isInvalid name) $ tellPsError (customError $ ErrOutOfScopeVariable name) l.locA -- XXX JB use proper error message
          pure e
        -- In HsDo, we can discard all in-scope variables in the context, since
        -- any !-desugaring we encounter cannot escape outside of this
        -- 'do'-block, and thus also not outside of the scope of those
        -- variables
        HsDo xd ctxt stmts -> L l . HsDo xd ctxt <$> local (const noneInScope) (traverse addStmts stmts)
        HsLet xl letTok binds inTok ex -> do
          (boundVars, binds') <- runWriter @OccSet $ evac binds
          fmap (L l . HsLet xl letTok binds' inTok) <$> liftMaybeT . local (addValids boundVars) $ evac ex

        -- TODO: check whether manually writing more cases here (espcially ones
        -- without expression where you can just return `pure e` improves
        -- performance)

        _ -> empty

    tryStmtLR :: forall a sig m . (Has Fill sig m, Data a) => a -> MaybeT m a
    tryStmtLR = try \e -> do
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
      (fromDList -> stmts, lstmt') <- runWriter $ evac lstmt
      pure $ map fromBindStmt stmts ++ [lstmt']

type HoleFills = Offer Loc LExpr
-- | We keep track of variables that are bound in lambdas, cases, etc., since
-- these are variables that will not be accessible in the surrounding
-- 'do'-block, and must therefore not be used.
-- The Reader is used to find out what local variables are in scope, the Writer
-- is used to inform callers which local variables have been bound.
type LocalVars = Reader InScope :+: Writer OccSet

-- TODO Do we really need Writer OccSet, Reader InScope, *and* State InScope?
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

tellOne :: Has (Writer (DList w)) sig m => w -> m ()
tellOne x = tell $ Endo (x:)

tellLocalVar :: Has (Writer OccSet) sig m => OccName -> m ()
tellLocalVar = tell . unitOccSet
