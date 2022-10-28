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

-- TODO: do we want to add Reader DynFlags with showPpr instead of using showPprUnsafe?

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
    else panic $ "Found extraneous bangs:" ++ unlines (showPprUnsafe <$> toList remainingErrs)
  where
    psError expr = \cases
      Preserve      -> PsErrBangPatWithoutSpace expr
      Don'tPreserve -> PsUnknownMessage $ DiagnosticMessage
        { diagMessage = mkDecorated [text "Monadic ! outside of a 'do'-block is not allowed"]
        , diagReason = ErrorWithoutFlag
        , diagHints = [SuggestMissingDo]
        }

        -- XXX JB seems like we probably only need one evac, not two
    evac :: forall a sig m . (Has Fill sig m, Data a) => a -> m a
    -- This recurses over all nodes in the AST, except for nodes for which
    -- one of the `try` functions returns `Just <something>`.
    evac e = maybe (gmapM evac e) pure =<< runMaybeT (tryEvac usualTries e)

    tryEvac :: Monad m => [a -> MaybeT m a] -> a -> MaybeT m a
    tryEvac tries = asum . (tries ??)

    -- TODO: Via benchmarking, find out whether it makes sense to `try` more
    -- datatypes here (e.g. `tryRdrName`) that would always remain unmodified
    -- anyway due to not containing expressions, and thus don't need to be
    -- recursed over
    usualTries :: (Has Fill sig m, Data a) => [a -> MaybeT m a]
    usualTries = [tryMatch, tryLExpr, tryStmt] -- XXX JB
    -- usualTries = [tryLExpr, tryStmt]

    -- We keep track of any local' binds, to prevent the user from using them
    -- with ! in situations where they would be evacuated to a place where
    -- they're not in scope
    -- XXX JB we need to make sure funbinds also add a local variable
    -- XXX JB but see XXX JB below about tryRdrName, that should fix it
    tryMatch :: forall a sig m . (Has Fill sig m, Data a) => a -> MaybeT m a
    tryMatch = try \(match@Match{ m_pats = pats
                                , m_grhss = grhss@GRHSs{ grhssGRHSs = grhssGrhss
                                                      , grhssLocalBinds = localBinds
                                                      }
                                } :: Match GhcPs LExpr) -> do
      traceM "doing match"
      -- We use the State to keep track of the bindings that have been
      -- introduced in patterns to the left of the one we're currently looking
      -- at. Example:
      --   \a (Just [b, (+ b) -> d]) (foldr a b -> c) | Just f <- b, f == 24
      -- the view pattern on `c` has access to the variables to the left of it. The same applies to `d`.
      -- `f == 24` additionally has access to variables defined in the guard to its left.
      (patVars, m_pats) <- ask @OccSet >>= runState ?? evacPats pats
      traceM $ "match patVars" ++ showPprUnsafe patVars
      traceM $ showPprUnsafe pats
      traceM $ showPprUnsafe match
      grhssLocalBinds <- local' (const patVars) $ evac localBinds
      grhssGRHSs <- evalState patVars $ evacPats grhssGrhss
      pure match{m_pats, m_grhss = grhss{grhssGRHSs, grhssLocalBinds}}

      where
        -- evacuate !s in pattern and collect all the names it binds
        evacPats :: forall a' m' sig' . (Has (Fill :+: State OccSet) sig' m', Data a') => a' -> m' a'
        evacPats e = do
          currentState <- get @OccSet
          maybe (gmapM evacPats e) pure =<< runMaybeT (tryEvac ((local' (const currentState) .) <$> (tryPat : usualTries)) e)

          where
            -- I think we should replace this by tryRdrName -- XXX JB
            tryPat :: forall a'' . Data a'' => a'' -> MaybeT m' a''
            tryPat = trace ("trying pattern " ++ show (typeRep (Nothing @a''))) try \(p :: Pat GhcPs) -> trace ("matching pattern " ++ showPprUnsafe p) case p of
              VarPat xv name -> tellName name $> VarPat xv name
              AsPat xa name pat -> do
                tellName name
                AsPat xa name <$> traverse (liftMaybeT . evacPats) pat

              _ -> empty
              where
                tellName name = trace "XXX TELLING" modify (extendOccSet ?? occName (unLoc name))

    tryLExpr :: forall a sig m . (Has Fill sig m, Data a) => a -> MaybeT m a
    tryLExpr = try \e@(L l _) -> do
      ExprLoc loc expr <- pure e
      case expr of
        -- Replace holes resulting from `!`
        -- If no corresponding expression can be found in the Offer, we assume
        -- that it was a hole put there by the user and leave it unmodified
        HsUnboundVar _ _ -> yoink loc >>= maybe (pure e) \lexpr -> do
          lexpr' <- evac lexpr
          name <- bangVar lexpr' loc
          tellOne $ name :<- lexpr'
          evac . L l $ HsVar noExtField (noLocA name)
        HsVar _ (occName . unLoc -> name) -> do
          traceM . showPprUnsafe =<< ask @OccSet
          whenM (elemOccSet name <$> ask) $ tellPsError PsErrUnpackDataCon l.locA -- XXX JB use proper error message
          pure e
        HsDo xd ctxt stmts -> L l . HsDo xd ctxt <$> local' (const emptyOccSet) (traverse addStmts stmts)

        -- TODO: check whether manually writing more cases here (espcially ones
        -- without expression where you can just return `pure e` improves
        -- performance)

        _ -> empty

    tryStmt :: forall a sig m . (Has Fill sig m, Data a) => a -> MaybeT m a
    tryStmt = try \e -> do
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
-- 'do'-block, and must therefore not be used.
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

-- XXX JB
local' :: forall a sig m r . (Outputable r, Has (Reader r) sig m) => (r -> r) -> m a -> m a 
local' f a = do
  traceM . ("XXX JB local'" ++). showPprUnsafe =<< ask @r
  local f a
