module MonadicBang.Test.ShouldFail where

import Control.Monad.Trans.Except

import MonadicBang.Test.Utils
import MonadicBang.Test.Utils.RunGhcParser

import GHC

import Debug.Trace
import GHC.Utils.Outputable
import GHC.Parser.Errors.Types

shouldFail :: Test
shouldFail = combined

combined :: Test
combined = do
  -- XXX JB should we add "main = " automatically? Might have to think about indentation, i.e. add 7 spaces to everything
  -- _test <- assertParseFailWith [] "!main = !do let a = b in pure !a !b"
  _test <- assertParseFailWith [PsWarnTab 2] "main = do let a = b in pure a b"
  pure ()
-- combined = runGhcParser "\
-- \!do\n\
-- \  x <- getA\n\
-- \  let y = let x = print 24 in !x\n\
-- \  let f (a, b) = !(f a) + !(let c = c + b in c + b + z)\n\
-- \  pure y\n\
-- \"
