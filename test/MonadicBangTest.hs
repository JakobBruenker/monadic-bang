{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}

{-# OPTIONS -fplugin=MonadicBang -fplugin-opt=MonadicBang:-ddump #-}

module Main (main) where

import GHC.Stack
import Data.Char

-- getA, getB, getC :: IO String
-- getA = pure "a"
-- getB = pure "b"
-- getC = pure "c"

main :: IO ()
main = do
  pure () -- XXX JB
--   withoutDo
--   insideDo
--   insideMDo
--   insideRec
--   nested
--   lambda
--   insideLet
--   listComp
--   monadComp
--   parListComp
--   multiWayIf
--   guards
--   viewPat
--   insideWhere
--   insideCase

-- assertEq :: (HasCallStack, Show a, Eq a) => a -> a -> IO ()
-- assertEq expected actual
--   | expected == actual = pure ()
--   | otherwise = withFrozenCallStack do
--       error $ "Expected " <> show expected <> ", but got " <> show actual

type Test = HasCallStack => IO ()

-- withoutDo :: Test
-- withoutDo = do assertEq "a" !getA

-- insideDo :: Test
-- insideDo = do
--   let ioA = getA
--       nonIOC = !getC
--   assertEq "abc" (!ioA ++ !ioB ++ nonIOC)
--   where
--     ioB = getB

-- insideMDo :: Test
-- insideMDo = assertEq (Just $ replicate @Int 10 -1) $ take 10 <$> mdo
--   xs <- Just (1:xs)
--   pure (negate <$> !(pure xs))

-- insideRec :: Test
-- insideRec = assertEq (Just $ take @Int 10 $ cycle [1, -1]) $ take 10 <$> do
--   rec xs <- Just (1:ys)
--       ys <- pure (negate <$> !(pure xs))
--   pure xs

-- nested :: Test
-- nested = do assertEq "Ab"
--                      !(pure (!(fmap toUpper <$> !(pure getA)) ++ !(!(pure getB))))

-- lambda :: Test
-- lambda = do assertEq "abc!" $ ((\a -> a ++ !getB) !getA) ++ !((\c -> do pure (!c ++ "!")) getC)

-- insideLet :: Test
-- insideLet = do
--   assertEq "abc" !do
--     let a = !getA
--     let b _ = !getB
--     let c = !getC in pure (a ++ b b ++ c)

-- listComp :: Test
-- listComp = assertEq @[Int]
--   [101, 102, 103, 201, 202, 203, 301, 302, 303]
--   [ ![1,2,3] + y | let y = ![100,200,300] ]

-- monadComp :: Test
-- monadComp = do assertEq "abc" ![ !getA ++ b ++ c | let b = !getB, c <- getC ]

-- parListComp :: Test
-- parListComp = assertEq @[Int]
--   [11111, 21111, 12111, 22111, 11221, 21221, 12221, 22221]
--   [ x + y + w + ![1000,2000] + ![10000,20000] | let x = ![1,2], let w = ![10,20] | let y = ![100,200] ]

-- guards :: Test
-- guards | [2,3,4] <- do [![1,2,3] + 1 :: Int] = pure ()
--        | otherwise = error "guards didn't match"

-- viewPat :: Test
-- viewPat = assertEq 9999 x
--   where (do pure (!succ * !pred) -> x) = 100 :: Int

-- insideWhere :: Test
-- insideWhere = do
--   c <- getC
--   assertEq "[2,3,4]c" $ show list ++ c
--   where
--     list = do [![1,2,3] + 1 :: Int]

-- insideCase :: Test
-- insideCase = do
--   assertEq "b"
--     case !getA of
--       (!(pure (++ "_")) -> "d") -> c ++ s123
--         where c = !getC
--               s123 = do pure !"123"
--       "c" -> "d"
--       _a -> "b"

-- multiWayIf :: Test
-- multiWayIf = do
--   assertEq "b" if
--     | !getA == !getA -> !getB
--     | otherwise      -> !getC

shouldFail :: Test
shouldFail = do
  let y = let x = print 24 in !x
  let f (a, b) = !a + !b
  pure y
