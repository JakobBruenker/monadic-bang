{-# LANGUAGE BlockArguments #-}

module Main (main) where

import GHC.Stack
import System.Environment
import System.Exit

main :: IO ()
main = do
  bangWithoutDo

assertEq :: (HasCallStack, Show a, Eq a) => a -> a -> IO ()
assertEq expected actual
  | expected == actual = return ()
  | otherwise = withFrozenCallStack do
      error $ "Expected " <> show expected <> ", but got " <> show actual

bangWithoutDo :: HasCallStack => IO ()
bangWithoutDo = assertEq "monadic-bang-test" !getProgName
