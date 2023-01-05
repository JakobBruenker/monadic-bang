{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE MonadComprehensions #-}

module Main (main) where

import MonadicBang.Test.ShouldPass
import MonadicBang.Test.ShouldFail

-- TODO: More tests with complex expressions inside the !

main :: IO ()
main = do
  shouldPass
  shouldFail
