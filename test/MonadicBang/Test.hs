{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import MonadicBang.Test.ShouldPass
import MonadicBang.Test.ShouldFail

-- TODO: More tests with complex expressions inside the !

main :: IO ()
main = do
  shouldPass
  shouldFail
