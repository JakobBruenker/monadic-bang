{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module MonadicBang.Internal where

import Prelude hiding (log)
import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Identity
import Control.Carrier.Reader
import Control.Carrier.Writer.Strict
import Control.Carrier.State.Strict
import Control.Carrier.Throw.Either
import Control.Carrier.Lift
import Control.Effect.Sum hiding (L)
import Control.Exception hiding (try, handle, Handler)
import Data.Data
import Data.Foldable
import Data.Functor
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Monoid
import GHC hiding (Type)
import GHC.Data.Bag
import GHC.Data.Maybe
import GHC.Parser.Errors.Types
import GHC.Plugins hiding (Type, Expr, empty, (<>), panic, try)
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

import Data.Kind

-- We don't care about which file things are from, because the entire AST comes
-- from the same module
data Loc = MkLoc {line :: Int, col :: Int}
         deriving (Eq, Ord, Show)

type Expr = HsExpr GhcPs
type LExpr = LHsExpr GhcPs

-- | To keep track of which local variables in scope may be used
--
-- If local variables are defined within the same statement as a !, but outside
-- of that !, they must not be used within this !, since their desugaring would
-- make them escape their scope.
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

replaceBangs :: [CommandLineOption] -> ModSummary -> Handler Hsc ParsedResult
replaceBangs cmdLineOpts _ (ParsedResult (HsParsedModule mod' files) msgs) = do
  options <- liftIO . (either throwIO pure =<<) . runThrow @ErrorCall $ parseOptions mod' cmdLineOpts
  traceShow cmdLineOpts $ pure ()
  -- TODO since the output of the Writer is not used, we should add a carrier providing evalWriter, which just ignores `tell`.
  dflags <- getDynFlags
  (newErrors, mod'') <- runM . runUniquesIO 'p' . runWriter . runReader options . runReader noneInScope . fmap snd . runWriter @OccSet . runReader dflags $ fillHoles fills mod'
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
                  
type HandleFailure :: Bool -> (Type -> Type) -> (Type -> Type)
type family HandleFailure canFail = t | t -> canFail where
  HandleFailure True = MaybeT
  HandleFailure False = IdentityT

class MonadTrans t => HandlingMonadTrans t where
  toMaybeT :: Monad m => t m a -> MaybeT m a

instance HandlingMonadTrans IdentityT where
  toMaybeT = MaybeT . fmap Just . runIdentityT 

instance HandlingMonadTrans MaybeT where
  toMaybeT = id

class Typeable (AstType a) => Handle a where
  type CanFail a :: Bool
  type AstType a = (r :: Type) | r -> a
  type Effects a :: (Type -> Type) -> Type -> Type
  handle' :: forall sig m m' . m ~ HandleFailure (CanFail a) m' => Has (Effects a) sig m' => Handler m (AstType a)

handle :: forall a sig m . (Handle a, CanFail a ~ False) => Has (Effects a) sig m => Handler m (AstType a)
handle = runIdentityT . handle'

try :: forall e sig m a .
       (HandlingMonadTrans (HandleFailure (CanFail e)), Typeable a, Handle e, Monad m, Has (Effects e) sig m) =>
       Handler (MaybeT m) a
try x = do
  Refl <- hoistMaybe $ eqT @a @(AstType e)
  toMaybeT $ handle' x

instance Handle GRHSs where
  type CanFail GRHSs = False
  type AstType GRHSs = GRHSs GhcPs LExpr
  type Effects GRHSs = Fill
  handle' grhss = do
    patVars <- ask @InScope
    grhssLocalBinds <- local (<> patVars) $ evac grhss.grhssLocalBinds
    grhssGRHSs <- evalState patVars $ evacPats grhss.grhssGRHSs
    pure grhss{grhssGRHSs, grhssLocalBinds}

instance Handle MatchGroup where
  type CanFail MatchGroup = False
  type AstType MatchGroup = MatchGroup GhcPs LExpr
  type Effects MatchGroup = Fill
  handle' mg = do
    mg_alts <- (traverse . traverse . traverse) handle mg.mg_alts
    pure mg{mg_alts}

instance Handle Match where
  type CanFail Match = False
  type AstType Match = Match GhcPs LExpr
  type Effects Match = Fill
  handle' match = do
    -- We use the State to keep track of the bindings that have been
    -- introduced in patterns to the left of the one we're currently looking
    -- at. Example:
    --
    -- > \a (Just [b, (+ b) -> d]) (foldr a b -> c) | Just f <- b, f == 24
    --
    -- the view pattern on `c` has access to the variables to the left of it. The same applies to `d`.
    -- `f == 24` additionally has access to variables defined in the guard to its left.
    (patVars, m_pats) <- ask @InScope >>= runState ?? evacPats match.m_pats
    m_grhss <- local (<> patVars) $ handle match.m_grhss
    pure match{m_pats, m_grhss}

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
--
-- If only the first `s` were defined, i.e.
--
-- > do let s = pure "outer"
-- >    putStrLn !s
--
-- it would be valid code.

instance Handle HsBindLR where
  type CanFail HsBindLR = True
  type AstType HsBindLR = HsBindLR GhcPs GhcPs
  type Effects HsBindLR = Fill
  handle' bind = case bind of
    FunBind{fun_id = occName . unLoc -> name, fun_matches = matches} -> do
      tellLocalVar name
      fun_matches <- local (addValid name) $ handle matches
      pure bind{fun_matches}
    PatBind{pat_lhs = lhs, pat_rhs = rhs} -> do
      (binds, pat_lhs) <- ask @InScope >>= flip runState (traverse evacPats lhs)
      pat_rhs <- local (<> binds) $ handle rhs
      pure bind{pat_lhs, pat_rhs}
    -- All VarBinds are introduced by the type checker, but we might as well handle them
    VarBind{var_id = occName -> name, var_rhs = expr} -> do
      tellLocalVar name
      var_rhs <- local (addValid name) $ evac expr
      pure bind{var_rhs}
    -- Pattern synonyms can never appear inside of do blocks, so we don't have
    -- to handle them specially
    PatSynBind{} -> empty

instance Handle Pat where
  type CanFail Pat = True
  type AstType Pat = Pat GhcPs
  type Effects Pat = Fill :+: State InScope
  handle' = \case
    VarPat xv name -> tellName name $> VarPat xv name
    AsPat xa name pat -> do
      tellName name
      AsPat xa name <$> traverse (liftMaybeT . evacPats) pat

    _ -> empty
    where
      tellName (occName . unLoc -> name) = do
        tellLocalVar name
        modify $ addValid name

instance Handle HsExpr where
  type CanFail HsExpr = True
  type AstType HsExpr = GenLocated SrcSpanAnnA Expr
  type Effects HsExpr = Fill
  handle' e@(L l _) = do
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
        pure . L l $ HsVar noExtField (noLocA name)
      HsVar _ (occName . unLoc -> name) -> do
        whenM (asks @InScope $ isInvalid name) $ tellPsError (customError $ ErrOutOfScopeVariable name) l.locA
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

instance Handle StmtLR where
  type CanFail StmtLR = True
  type AstType StmtLR = StmtLR GhcPs GhcPs LExpr
  type Effects StmtLR = Fill
  handle' :: forall sig m m' . (m ~ MaybeT m', Has (Effects StmtLR) sig m') => Handler m (AstType StmtLR)
  handle' e = case e of

    RecStmt{recS_stmts} -> do
      recS_stmts' <- traverse addStmts recS_stmts
      pure e{recS_stmts = recS_stmts'}
    ParStmt xp stmtBlocks zipper bind -> do
      stmtsBlocks' <- traverse addParStmts stmtBlocks
      pure $ ParStmt xp stmtsBlocks' zipper bind
      where
        addParStmts :: Handler m (ParStmtBlock GhcPs GhcPs)
        addParStmts (ParStmtBlock xb stmts vars ret) = do
          stmts' <- addStmts stmts
          pure $ ParStmtBlock xb stmts' vars ret

    _ -> empty

-- | Replace holes in an AST whenever an expression with the corresponding
-- source span can be found in the given list.
fillHoles :: (Data a, Has (PsErrors :+: Reader Options :+: Uniques :+: LocalVars :+: Reader DynFlags) sig m) => Map Loc LExpr -> Handler m a
fillHoles fillers ast = do
  (remainingErrs, (fromDList -> binds :: [BindStmt], ast')) <- runOffer fillers . runWriter $ evac ast
  MkOptions{preserveErrors} <- ask
  for_ binds \bind -> tellPsError (psError (bindStmtExpr bind) preserveErrors) (bangSpan $ bindStmtSpan bind)
  dflags <- ask
  pure if null remainingErrs
    then ast'
    else panic $ "Found extraneous bangs:" ++ unlines (showPpr dflags <$> toList remainingErrs)
  where
    psError expr = \cases
      Preserve      -> PsErrBangPatWithoutSpace expr
      Don'tPreserve -> customError ErrBangOutsideOfDo

evac :: forall a sig m . (Has Fill sig m, Data a) => Handler m a
-- This recurses over all nodes in the AST, except for nodes for which
-- one of the `try` functions returns `Just <something>`.
evac e = maybe (gmapM evac e) pure =<< runMaybeT (tryEvac usualTries e)

tryEvac :: Monad m => [Handler (MaybeT m) a] -> Handler (MaybeT m) a
tryEvac tries = asum . (tries ??)

-- TODO: Via benchmarking, find out whether it makes sense to `try` more
-- datatypes here (e.g. `trySrcSpan`) that would always remain unmodified
-- anyway due to not containing expressions, and thus don't need to be
-- recursed over (in that case you could probably have a function like
-- `ignore @SrcSpan`, which would simplify things)
-- TODO sort _roughly_ by how often each try will succeed (micro-optimization)
usualTries :: (Has Fill sig m, Data a) => [Handler (MaybeT m) a]
usualTries = [try @HsBindLR, try @MatchGroup, try @HsExpr, try @StmtLR]

-- | evacuate !s in pattern and collect all the names it binds
evacPats :: forall a m sig . (Has (Fill :+: State InScope) sig m, Data a) => Handler m a
evacPats e = do
  currentState <- get @InScope
  maybe (gmapM evacPats e) pure =<< runMaybeT (tryEvac ((local (<> currentState) .) <$> (try @Pat : usualTries)) e)

-- | Find all !s in the given statements and combine the resulting bind
-- statements into lists, with the original statements being the last one
-- in each list - then concatenate these lists
addStmts :: forall sig m . Has (PsErrors :+: HoleFills :+: Uniques :+: LocalVars :+: Reader DynFlags) sig m => Handler m [ExprLStmt GhcPs]
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

type Fill = PsErrors :+: Writer (DList BindStmt) :+: HoleFills :+: Uniques :+: LocalVars :+: Reader DynFlags

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

-- | Use the !'d expression if it's short enough, or else abbreviate with `...`
-- We don't need to worry about shadowing other !'d expressions:
-- - For the user, we add line and column numbers to the name
-- - For the compiler, we use a unique instead of the name
bangVar :: Has (Uniques :+: Reader DynFlags) sig m => LExpr -> Loc -> m RdrName
bangVar (L spn expr) loc = do
  dflags <- ask
  let name = '!' : case lines (showPpr dflags expr) of
        (str:rest) | null rest && length str < 20 -> str
                   | otherwise                    -> take 16 str ++ "..."
        _                                         -> "<empty expression>"
  locVar name spn.locA loc

locVar :: Has Uniques sig m => String -> SrcSpan -> Loc -> m RdrName
locVar str spn loc = do
  let occ = mkVarOcc $ printf "<%s:%d:%d>" str loc.line loc.col
  unique <- freshUnique
  pure . nameRdrName $ mkInternalName unique occ spn

tellOne :: Has (Writer (DList w)) sig m => w -> m ()
tellOne x = tell $ Endo (x:)

tellLocalVar :: Has (Writer OccSet) sig m => OccName -> m ()
tellLocalVar = tell . unitOccSet
