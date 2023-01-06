module MonadicBang (plugin) where

import GHC.Plugins
import MonadicBang.Internal

-- TODO: Write user manual as haddock comment

-- TODO: mention in the documentation how unfortunately you get a parse error for each exclamation mark if you get a fatal parse error

-- TODO mention in documentation how shadowing is an issue -- update: I don't think that's a thing anymore with State?

-- TODO mention in docs how `do put 4; put 5 >> print !get` will print 4, not 5? whereas `do put 4; put 5; print !get` *will* produce 5

-- TODO mention in docs how `<-` becomes redundant if you want it to (because you can use let + ! instead)

-- TODO make sure it works in HLS without -fplugin-package. If not, tell people to use it.

plugin :: Plugin
plugin = defaultPlugin
  { parsedResultAction = replaceBangs
  , pluginRecompile = purePlugin
  }
