-- | GHC plugin to desugar ! into do-notation
--
-- For more information, please refer to the README.
module MonadicBang (plugin) where

import GHC.Plugins
import MonadicBang.Internal

plugin :: Plugin
plugin = defaultPlugin
  { parsedResultAction = replaceBangs
  , pluginRecompile = purePlugin
  }
