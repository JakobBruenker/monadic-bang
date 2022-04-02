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

import Control.Arrow
import Data.Data
import Data.Function
import Data.Functor
import GHC
import GHC.Data.Bag
import GHC.Parser.Errors.Types
import GHC.Plugins
import GHC.Types.Error

import Debug.Trace

plugin :: Plugin
plugin = defaultPlugin
  { parsedResultAction = replaceBangs
  , pluginRecompile = purePlugin
  }

-- We don't care about which file things are from, because the entire AST comes
-- from the same module
data Loc = MkLoc {line :: !Int, col :: Int}
         deriving (Eq, Ord, Show)

-- | Increment column by one to get the location after a bang
dropBang :: Loc -> Loc
dropBang loc = loc{col = loc.col + 1}

-- | Used to extract the Loc of a located expression
pattern ExprLoc :: Loc -> HsExpr GhcPs -> LHsExpr GhcPs
pattern ExprLoc{loc, expr} <- L (locA -> RealSrcSpan (spanToLoc -> loc) _) expr

spanToLoc :: RealSrcSpan -> Loc
spanToLoc = uncurry MkLoc . (srcLocLine &&& srcLocCol) . realSrcSpanStart

replaceBangs :: [CommandLineOption] -> ModSummary -> ParsedResult -> Hsc ParsedResult
replaceBangs _ _ (ParsedResult (HsParsedModule lexp files) msgs) =
  pure $ ParsedResult (HsParsedModule (fillHoles fills lexp) files) msgs{psErrors}
  where
    -- Take out the errors we care about, throw the rest back in
    (mkMessages -> psErrors, bagToList -> fills) =
      flip partitionBagWith msgs.psErrors.getMessages \cases
        err
          | PsErrBangPatWithoutSpace ExprLoc{loc, expr} <- err.errMsgDiagnostic
          -> traceShow loc $ Right (loc, expr)
          | otherwise -> Left err

-- | Replace holes in an AST whenever an expression with the corresponding
-- source span can be found in the given list.
--
-- Expressions to fill them with are taken from the errors.
-- TODO: embed the expression in existing or new do-notation
-- Approach: in tryFillHole, whenever we encounter let/where/do/etc.,
-- make a separate monadic traversal through the subtree with a writer monad,
-- adding the bindings we need to the monadic context so we can construct the
-- correct do block once the traversal is evaluated
-- TODO for large modules with lots of !s, this might be slightly faster if we
-- only consider bangs we haven't found yet, i.e. remove others from the list.
fillHoles :: forall a . Data a => [(Loc, HsExpr GhcPs)] -> a -> a
fillHoles exprs = gmapT \cases
  (e :: e) -> eqT @e @(LHsExpr GhcPs) & \cases
    (Just Refl)
      | lexp@(ExprLoc (dropBang -> loc) (HsUnboundVar _ _)) <- e
      , Just expr <- lookup loc exprs
      -> fillHoles exprs (lexp $> expr)
    _ -> fillHoles exprs e
