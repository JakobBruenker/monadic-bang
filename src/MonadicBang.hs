module MonadicBang (plugin) where

import GHC.Plugins
import MonadicBang.Internal

-- TODO: Write user manual as haddock comment

-- TODO: mention in the documentation how unfortunately you get a parse error for each exclamation mark if you get a fatal parse error

-- TODO mention in documentation how shadowing is an issue

-- TODO mention in docs how `do put 4; put 5 >> print !get` will print 4, not 5? whereas `do put 4; put 5; print !get` *will* produce 5

-- TODO mention in docs how `<-` becomes redundant if you want it to

-- TODO Consider writing a frontend plugin for versions <9.4
-- The idea would be to replace all instances of ! (not adjacent to other
-- symbols) with (:!), and then hopefully we can pick up on that in the parser
-- and convert all (:!)s to what they should be, bang patterns or strictness
-- anotations or MonadicBang.
-- If the token following it is a single word, the whole expression must be
-- surrounded by parentheses as well, e.g. ((:!)Int)
-- Annoyingly, this is also true for parenthesized statements. i.e.
-- !(a + !(!b + c)) has to be converted into ((:!)(a + ((:!)(((:!)b + c))))) :/
-- (The ghc flags you need are -F -pgmF, maybe -optF for arguments)
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/phases.html#pre-processor
-- Might need to generate a LINE pragma like in the link above to keep the right filename in error messages
-- (cabal flag is build-tool-depends)
-- Luckily I think writing a pasrser just for ! and balanced parentheses is not *that* difficult.

-- TODO make sure it works in HLS without -fplugin-package. If not, tell people to use it.

plugin :: Plugin
plugin = defaultPlugin
  { parsedResultAction = replaceBangs
  , pluginRecompile = purePlugin
  }
