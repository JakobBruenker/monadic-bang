{-# LANGUAGE BlockArguments #-}

module MonadicBang.Test.Utils where

import GHC.Stack

assertEq :: (HasCallStack, Show a, Eq a) => a -> a -> IO ()
assertEq expected actual
  | expected == actual = pure ()
  -- We dont' care about seeing where the `error` call itself happens in the
  -- call stack, so we freeze it
  | otherwise = withFrozenCallStack do
      error $ "Expected " <> show expected <> ", but got " <> show actual

type Test = HasCallStack => IO ()
