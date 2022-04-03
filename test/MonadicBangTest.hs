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

-- TODO:
-- let; in Idris the do block is around the entire let expression I don't know if I like that though?
--    There's an infelicity here: on the one hand, it's really useful to have ! not introduce a new do-block inside let inside do
--    On the other hand, it would be nice if let inside do worked like let...in, and if that worked like where
--    But where *has* to work like top-level function definitions
--    In any case, it's probably a good idea to stick to the idris conventions for now
-- where
-- multiply nested
-- mdo
-- rec
-- do
-- lambda
-- case scrutinee
-- case body
-- case where (treat the same as top level?)
-- view pattern -> seems kinda hard but doable (that is on top level, apart from that it's the same as everything else)
-- let inside do; In idris this uses the existing do-block, so
-- do let a = !getLine
--    print a
-- here (a :: String), *not* (a :: IO String)

-- The one case which I think I won't handle like idris is that for us, a bare !x expression at top level will be treated as do {x' <- x; pure x}
-- which is equivalent to x. It's a type error in idris. Alternatively we could make it a parse error... since it's not like there's any point in doing it.
