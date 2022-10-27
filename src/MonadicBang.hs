module MonadicBang (plugin) where

import GHC.Plugins
import MonadicBang.Internal

-- TODO: Write user manual as haddock comment

-- TODO: mention in the documentation how unfortunately you get a parse error for each exclamation mark if you get a fatal parse error

-- TODO mention in documentation how shadowing is an issue

plugin :: Plugin
plugin = defaultPlugin
  { parsedResultAction = replaceBangs
  , pluginRecompile = purePlugin
  }
