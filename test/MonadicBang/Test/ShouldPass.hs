{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE NegativeLiterals #-}

{-# OPTIONS_GHC -fplugin=MonadicBang #-}

module MonadicBang.Test.ShouldPass where
 
import Data.Char
import Control.Monad.Trans.State

import MonadicBang.Test.Utils
import MonadicBang.Test.Utils.QualifiedDo qualified as QualifiedDo

shouldPass :: Test
shouldPass = do
  simpleDo
  insideDo
  insideMDo
  insideRec
  nested
  lambda
  insideLet
  listComp
  monadComp
  parListComp
  multiWayIf
  guards
  viewPat
  insideWhere
  insideCase
  usingDoBlockVar
  largeExpr
  confusing
  qualifiedDo

getA, getB, getC :: IO String
getA = pure "a"
getB = pure "b"
getC = pure "c"

simpleDo :: Test
simpleDo = do assertEq "a" !getA

insideDo :: Test
insideDo = do
  let ioA = getA
      nonIOC = !getC
  assertEq "abc" (!ioA ++ !ioB ++ nonIOC)
  where
    ioB = getB

insideMDo :: Test
insideMDo = assertEq (Just $ replicate @Int 10 -1) $ take 10 <$> mdo
  xs <- Just (1:xs)
  pure (negate <$> !(pure xs))

insideRec :: Test
insideRec = assertEq (Just $ take @Int 10 $ cycle [1, -1]) $ take 10 <$> do
  rec xs <- Just (1:ys)
      ys <- pure (negate <$> !(pure xs))
  pure xs

nested :: Test
nested = do assertEq "Ab"
                     !(pure (!(fmap toUpper <$> !(pure getA)) ++ !(!(pure getB))))

lambda :: Test
lambda = do assertEq "abc!" $ ((\a -> a ++ !getB) !getA) ++ !((\c -> do pure (!c ++ "!")) getC)

insideLet :: Test
insideLet = do
  assertEq "abc" !do
    let a = !getA
    let b _ = !getB
    let c = !getC in pure (a ++ b b ++ c)

listComp :: Test
listComp = assertEq @[Int]
  [101, 102, 103, 201, 202, 203, 301, 302, 303]
  [ ![1,2,3] + y | let y = ![100,200,300] ]

monadComp :: Test
monadComp = do assertEq "abc" ![ !getA ++ b ++ c | let b = !getB, c <- getC ]

parListComp :: Test
parListComp = assertEq @[Int]
  [11111, 21111, 12111, 22111, 11221, 21221, 12221, 22221]
  [ x + y + w + ![1000,2000] + ![10000,20000] | let x = ![1,2], let w = ![10,20] | let y = ![100,200] ]

guards :: Test
guards | [2,3,4] <- do [![1,2,3] + 1 :: Int] = pure ()
       | otherwise = error "guards didn't match"

viewPat :: Test
viewPat = assertEq 9999 x
  where (do pure (!succ * !pred) -> x) = 100 :: Int

insideWhere :: Test
insideWhere = do
  c <- getC
  assertEq "[2,3,4]c" $ show list ++ c
  where
    list = do [![1,2,3] + 1 :: Int]

insideCase :: Test
insideCase = do
  assertEq "b"
    case !getA of
      (!(pure (++ "_")) -> "d") -> c "abc" ++ s123
        where c a = !getC ++ a
              s123 = do pure !"123"
      "c" -> "d"
      _a -> "b"

multiWayIf :: Test
multiWayIf = do
  assertEq "b" if
    | !getA == !getA -> !getB
    | otherwise      -> !getC

usingDoBlockVar :: Test
usingDoBlockVar = do
  let a = !getA
  assertEq "a" !(pure a)

largeExpr :: Test
largeExpr = do
  assertEq () !(assertEq () !(assertEq "abc" ![ !getA ++ b ++ c | let b = !getB, c <- getC ]))

confusing :: Test
confusing = do
  assertEq @Int 4 $ flip evalState 0 do
    put 4
    put 5 >> pure !get
  assertEq @Int 5 $ flip evalState 0 do
    put 4
    put 5
    pure !get 

qualifiedDo :: Test
qualifiedDo = do
  assertEq (5 + 10 + 20 + (5 + 20)) QualifiedDo.do
    x <- 5
    10
    y <- 20
    x + y
