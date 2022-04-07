{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LexicalNegation #-}

module Main (main) where

import GHC.Stack
import System.Environment

progName :: String
progName = "monadic-bang-test"

main :: IO ()
main = withProgName progName do
  bangWithoutDo
  bangInsideDo
  bangInsideMDo
  bangInsideRec
  bangNested

assertEq :: (HasCallStack, Show a, Eq a) => a -> a -> IO ()
assertEq expected actual
  | expected == actual = pure ()
  | otherwise = withFrozenCallStack do
      error $ "Expected " <> show expected <> ", but got " <> show actual

bangWithoutDo :: HasCallStack => IO ()
bangWithoutDo = assertEq progName !getProgName

bangInsideDo :: HasCallStack => IO ()
bangInsideDo = do
  let ioProgNameA = getProgName
  assertEq (progName ++ progName) (!ioProgNameA ++ !ioProgNameB)
  where
    ioProgNameB = getProgName

bangInsideMDo :: HasCallStack => IO ()
bangInsideMDo = assertEq (Just $ replicate @Int 10 -1) $ take 10 <$> mdo
  xs <- Just (1:xs)
  pure (negate <$> !(pure xs))

bangInsideRec :: HasCallStack => IO ()
bangInsideRec = assertEq (Just $ take @Int 10 $ cycle [1, -1]) $ take 10 <$> do
  rec xs <- Just (1:ys)
      ys <- pure (negate <$> !(pure xs))
  pure xs

bangNested :: HasCallStack => IO ()
bangNested = assertEq (reverse progName ++ progName)
                      !(pure (!(reverse <$> !(pure getProgName)) ++ !(!(pure getProgName))))

-- DONE:
-- do
-- mdo
-- rec
-- multiply nested

-- TODO:
-- let; in Idris the do block is around the entire let expression I don't know if I like that though?
--    There's an infelicity here: on the one hand, it's really useful to have ! not introduce a new do-block inside let inside do
--    On the other hand, it would be nice if let inside do worked like let...in, and if that worked like where
--    But where *has* to work like top-level function definitions
--    In any case, it's probably a good idea to stick to the idris conventions for now
-- where
-- list/monad comprehension (treat like do? idris does.)
-- lambda
-- case scrutinee
-- case body
-- case where (treat the same as top level? That's how idris does it)
-- view pattern -> seems kinda hard but doable (that is on top level, apart from that it's the same as everything else)
-- let inside do; In idris this uses the existing do-block, so
-- do let a = !getLine
--    print a
-- here (a :: String), *not* (a :: IO String)

-- The one case which I think I won't handle like idris is that for us, a bare !x expression at top level will be treated as do {x' <- x; pure x}
-- which is equivalent to x. It's a type error in idris. Alternatively we could make it a parse error... since it's not like there's any point in doing it.

-- You probably have to eta expand, i.e. you'll have to write `f a = (,) !b a` instead of `f = (,) !b` - at the top level. Not in `let`s though.
-- ^ this is also true in idris

-- Here's potentially a big problem: With (\x -> !x), we can't easily lift the expression outside of the lambda, because x is only defined inside the lambda.
-- This probably affects a whole bunch of other things, too... Like let: let f a b = !a
-- So we might have to deviate from Idris after all, and insert `do` at the top of lets/lambdas, unless we want to do something more fancy than just looking at syntax
-- (you could potentially distinguish between lets with pattern bindings and var bindings, but that seems somewhat ad-hoc)
-- Potentially you could analyze whether a given expression only uses variables that are in scope... I don't know this feels like it would get complicated to use, we'll see
-- => potential solution: Disallow using variables that are bound in lambda or let blocks without explicitly surrounding them by a do, this seems like maybe a good idea
--    Still would be more complicated than just syntax but not *too* much more complicated, just have to keep track of currently bound variables in the state
--    I mean effectively this is just the same as not doing anything fancy at all, but with better error messages, so from that point of view maybe it's okay because the fancy stuff doesn't actually change semantics
