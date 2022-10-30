module MonadicBang (plugin) where

import GHC.Plugins
import MonadicBang.Internal

-- TODO: Write user manual as haddock comment

-- TODO: mention in the documentation how unfortunately you get a parse error for each exclamation mark if you get a fatal parse error

-- TODO mention in documentation how shadowing is an issue

-- TODO mention in docs how `do put 4; put 5 >> print !get` will print 4, not 5? whereas `do put 4; put 5; print !get` *will* produce 5

-- TODO metion in docs how `<-` becomes redundant if you want it to

plugin :: Plugin
plugin = defaultPlugin
  { parsedResultAction = replaceBangs
  , pluginRecompile = purePlugin
  }
