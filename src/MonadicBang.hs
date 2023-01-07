-- | GHC plugin to desugar ! into do-notation
--
-- For more information, please refer to the README.
module MonadicBang (plugin) where

import GHC.Plugins
import MonadicBang.Internal

-- TODO: check whether you need to add anything to the cabal file (e.g. issue page)

plugin :: Plugin
plugin = defaultPlugin
  { parsedResultAction = replaceBangs
  , pluginRecompile = purePlugin
  }
