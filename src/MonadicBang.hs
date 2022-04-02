{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module MonadicBang (plugin) where

import Data.Data
import Data.Maybe
import Data.Type.Equality
import GHC.Plugins
import GHC

plugin :: Plugin
plugin = defaultPlugin
  { parsedResultAction = replaceBangs
  , pluginRecompile = purePlugin
  }

replaceBangs :: [CommandLineOption] -> ModSummary -> ParsedResult -> Hsc ParsedResult
replaceBangs _ _ (ParsedResult (HsParsedModule lexp files) msgs) =
  pure $ ParsedResult (HsParsedModule (fillHoles [] lexp) files) msgs

-- | Replace holes in an AST whenever an expression with the corresponding
-- source span can be found in the given list.
--
-- Expressions to fill them with are taken from the errors.
fillHoles :: forall a . Data a => [LHsExpr GhcPs] -> a -> a
fillHoles exprs = gmapT \cases
  (d :: d) -> fillHoles exprs (d `fromMaybe` tryHole)
    where
      tryHole :: Maybe d
      tryHole = eqT @d @(HsExpr GhcPs) >>= \case
        Refl | HsUnboundVar _ _ <- d -> Just $ error "ok"
        _ -> Nothing
