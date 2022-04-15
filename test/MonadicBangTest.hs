{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import GHC.Stack
import Data.Char

getA, getB, getC :: IO String
getA = pure "a"
getB = pure "b"
getC = pure "c"

main :: IO ()
main = do
  bangWithoutDo
  bangInsideDo
  bangInsideMDo
  bangInsideRec
  bangNested
  bangCase
  bangLambda
  bangLet
  bangListComp
  bangMonadComp
  bangParListComp
  bangGuards
  bangViewPat
  bangWhere

assertEq :: (HasCallStack, Show a, Eq a) => a -> a -> IO ()
assertEq expected actual
  | expected == actual = pure ()
  | otherwise = withFrozenCallStack do
      error $ "Expected " <> show expected <> ", but got " <> show actual

bangWithoutDo :: HasCallStack => IO ()
bangWithoutDo = assertEq "a" !getA

bangInsideDo :: HasCallStack => IO ()
bangInsideDo = do
  let ioA = getA
      nonIOC = !getC
  assertEq "abc" (!ioA ++ !ioB ++ nonIOC)
  where
    ioB = getB

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
bangNested = assertEq "Ab"
                      !(pure (!(fmap toUpper <$> !(pure getA)) ++ !(!(pure getB))))

bangCase :: HasCallStack => IO ()
bangCase = assertEq "b" case !getA of
  something | something == !getA -> !getB
  _ -> ""

bangLambda :: HasCallStack => IO ()
bangLambda = assertEq "abc!" $ ((\a -> a ++ !getB) !getA) ++ !((\c -> do pure (!c ++ "!")) getC)

bangLet :: HasCallStack => IO ()
bangLet = assertEq "abc" !do
  let a = !getA
  let b _ = !getB
  let c = !getC in pure (a ++ b b ++ c)

bangListComp :: HasCallStack => IO ()
bangListComp = assertEq @[Int]
  [101, 102, 103, 201, 202, 203, 301, 302, 303]
  [ ![1,2,3] + y | let y = ![100,200,300] ]

bangMonadComp :: HasCallStack => IO ()
bangMonadComp = assertEq "abc" ![ !getA ++ b ++ c | let b = !getB, c <- getC ]

bangParListComp :: HasCallStack => IO ()
bangParListComp = assertEq @[Int]
  [11111, 21111, 12111, 22111, 11221, 21221, 12221, 22221]
  [ x + y + w + ![1000,2000] + ![10000,20000] | let x = ![1,2], let w = ![10,20] | let y = ![100,200] ]

bangGuards :: HasCallStack => IO ()
bangGuards | [2,3,4] <- [![1,2,3] + 1 :: Int] = pure ()
           | otherwise = error "guards didn't match"

bangViewPat :: HasCallStack => IO ()
bangViewPat = assertEq 9999 x
  where (pure (!succ * !pred) -> x) = 100 :: Int

bangWhere :: HasCallStack => IO ()
bangWhere = do
  c <- getC
  assertEq "[2,3,4]c" $ show list ++ c
  where
    list = [![1,2,3] + 1 :: Int]

-- DONE:
-- guards
-- do
-- mdo
-- rec
-- multiply nested
-- case scrutinee
-- case body
-- lambda
-- let; in Idris the do block is around the entire let expression I don't know if I like that though?
--    There's an infelicity here: on the one hand, it's really useful to have ! not introduce a new do-block inside let inside do
--    On the other hand, it would be nice if let inside do worked like let...in, and if that worked like where
--    But where *has* to work like top-level function definitions
--    In any case, it's probably a good idea to stick to the idris conventions for now
-- let inside do; In idris this uses the existing do-block, so
--   do let a = !getLine
--      print a
--   here (a :: String), *not* (a :: IO String)
-- list/monad comprehension (treat like do? idris does.) NB: we do things in the "last statement" last, even though they are leftmost (same as in Idris)
-- view pattern seem kinda hard but doable (that is on top level, apart from that it's the same as everything else)
--   Oop I actually don't think so: While applying the view patterns we don't know yet which guard alternative we're in, so in which one do we put the do?
--   So that means we just treat them like everything else
-- where (i.e. handle GRHSs)
-- case where (treat the same as top level? That's how idris does it) (also handled by GRHSs)

-- TODO:
-- empty \o/
-- (however there are still things to do, see below)

-- Hmm in idris case alternatives seem to start a new do block. We're not currently doing it, but it's certainly worth considering.
-- As usual I think not automatically starting a new do block offers the user more freedom, but it *might* be more intuitive to do it anyway, since that means only the effects in alternatives that actually happen are
-- executed. I suppose the same applies to if/multiway-if. But then, you could also say the same about let, since it can use guards and whatnot. So I'm not convinced - though I do think it's more intuitive...
-- Idris doesn't have the let problem, because it doesn't have guards on let
-- One thing one could consider is not starting a new block in let, *unless* there are guards, maybe not the worst idea. Of course in that case, we should also think about the right approach for lambdas again.
-- But then you should also think about things like `let f x = !x + 1`. Like, should that be different from `let f = \x -> !x + 1`? Maybe.
-- Perhaps a good heuristic is to think about what you'd expect in a (strict) imperative language.
-- I mean, you could also just be super consistent and say if you want to have something like let without initiating a do block, you have to write
-- Here's one cool idea: Only don't start a new do block if it's a strict pattern/variable binding
-- btw `let (_, _) = undefined` is different from `let !(_, _) = undefined`, I didn't know.
-- because those *have* to have their body evaluated. Still not sure then about guards though. Best guess is that should still start a do block.
-- But at that point maybe it'd just be better to not care about strict/non-strict and just do it for pattern/var bindings in general.
-- x <- pure ... instead of let x = ....
-- But if we decide to do it differently from idris, we should certainly list it as a difference.

-- You could also go in a completely different direction and handle it more like arrow desugaring, which, while fairly complex, is almost what I'm leaning towards:
--
-- if you have
--   let x = if cond
--     then !a
--     else !b + !c
--
-- then this would be desugared into
--
--   if_res <- if cond
--     then Left <$> a
--     else Right <$> (#b, c#)
--   let x = case if_res of
--     Left a -> a
--     Right (#b, c#) -> b + c
--
-- The one caveat here is that this doesn't take care of laziness, i.e. the effects are still run even  if x isn't used, but maybe that's okay.
-- Question: Is this only applicable to guards/if/case, or also to lambdas/functions?
-- I'm fairly confident that this does not apply to lambdas. So then you would have:
-- - lambdas/lets with at least one argument start a new do-block
-- - if/case/multiway-if/let without arguments/guards don't start a do-block
-- I think I can live with that.

-- one case which I think we won't handle like idris is that for us, a bare !x expression at top level will be treated as do {x' <- x; pure x}
-- which is equivalent to x. It's a type error in idris. Alternatively we could make it a parse error... since it's not like there's any point in doing it.
-- Or - perhaps best - we could add a parse warning

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
-- Update from testing via idris: It looks like it can handle variables bound in lambdas, but *not* in let expressions. Do lambdas start a new do block?
-- I believe that the answer is this:
-- - Lambda always starts new do block
-- - let *only* starts new do block for declarations that have a type signature - which I don't like. Not a very intuitive rule.
-- Since we're already going to be deviating in the latter case - I really don't think type decs should make a difference - I suppose we might as well deviate in the former case.
-- Not automatically starting a do block in a lambda gives the user more choice: if they want that behavior, they can still start a do block manually.

-- You could keep track in a reader monad which variables were introduced together with how (e.g. via lambda, or via function definition, or via case pattern, etc.) and then tell the user
-- something along the lines of "The variable blah would escape its scope if we did this. Possible fix: Start a do block inside the lambda/function definition/case expression that blah"

-- We don't have any special handling for transform statements for now
-- Parallel list/monad comps are handled such that first all the parallel statements are done, and then this is treated as a regular in series statement with the last statement, including any bangs in the last statement.
