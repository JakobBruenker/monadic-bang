module MonadicBang.Test.ShouldFail where

import Control.Monad.Trans.Except

import MonadicBang.Test.Utils
import MonadicBang.Test.Utils.RunGhcParser

import GHC

import Debug.Trace
import GHC.Utils.Outputable

shouldFail :: Test
shouldFail = combined

combined :: Test
combined = do
  _test <- runExceptT $ parseGhc "do pure !a !b"
  traceM "XXXXXXXXXXXXXXXXXX"
  error $ either show showPprUnsafe $ pm_parsed_source <$> _test
  pure ()
-- combined = runGhcParser "\
-- \!do\n\
-- \  x <- getA\n\
-- \  let y = let x = print 24 in !x\n\
-- \  let f (a, b) = !(f a) + !(let c = c + b in c + b + z)\n\
-- \  pure y\n\
-- \"
