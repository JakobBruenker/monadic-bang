{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE MonadComprehensions #-}

module Main (main) where

import MonadicBang.Test.ShouldPass
import MonadicBang.Test.ShouldFail

main :: IO ()
main = do
  shouldPass
  shouldFail
