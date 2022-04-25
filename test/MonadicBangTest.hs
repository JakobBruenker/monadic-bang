{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import GHC.Stack
import Data.Char

getA, getB, getC :: IO String
getA = pure "a"
getB = pure "b"
getC = pure "c"

-- Binding to make sure we don't get any name collisions
(!) :: Int
(!) = 4

main :: IO ()
main = do
  withoutDo
  insideDo
  insideMDo
  insideRec
  nested
  lambda
  insideLet
  listComp
  monadComp
  parListComp
  guards
  viewPat
  insideWhere
  -- insideCase

assertEq :: (HasCallStack, Show a, Eq a) => a -> a -> IO ()
assertEq expected actual
  | expected == actual = pure ()
  | otherwise = withFrozenCallStack do
      error $ "Expected " <> show expected <> ", but got " <> show actual

type Test = HasCallStack => IO ()

withoutDo :: Test
withoutDo = assertEq "a" !getA

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
  pure (negate <$> !(pure @[] xs))

insideRec :: Test
insideRec = assertEq (Just $ take @Int 10 $ cycle [1, -1]) $ take 10 <$> do
  rec xs <- Just (1:ys)
      ys <- pure (negate <$> !(pure @[] xs))
  pure xs

nested :: Test
nested = assertEq "Ab"
                  !(pure @IO (!(fmap toUpper <$> !(pure @IO getA)) ++ !(!(pure @IO getB))))

lambda :: Test
lambda = assertEq "abc!" $ ((\a -> a ++ !getB) !getA) ++ !((\c -> do pure @IO (!c ++ "!")) getC)

insideLet :: Test
insideLet = assertEq "abc" !do
  let a = !getA
  let b _ = !getB
  let c = !getC in pure @IO (a ++ b b ++ c)

listComp :: Test
listComp = assertEq @[Int]
  [101, 102, 103, 201, 202, 203, 301, 302, 303]
  [ ![1,2,3] + y | let y = ![100,200,300] ]

monadComp :: Test
monadComp = assertEq "abc" ![ !getA ++ b ++ c | let b = !getB, c <- getC ]

parListComp :: Test
parListComp = assertEq @[Int]
  [11111, 21111, 12111, 22111, 11221, 21221, 12221, 22221]
  [ x + y + w + ![1000,2000] + ![10000,20000] | let x = ![1,2], let w = ![10,20] | let y = ![100,200] ]

guards :: Test
guards | [2,3,4] <- [![1,2,3] + 1 :: Int] = pure ()
           | otherwise = error "guards didn't match"

viewPat :: Test
-- TODO This is still defaulting, we'll have to see what happens here once the typechecking plugin is done
viewPat = assertEq (9999 :: Int) x
  where (pure (!succ * !pred) -> x) = 100

insideWhere :: Test
insideWhere = do
  c <- getC
  assertEq "[2,3,4]c" $ show list ++ c
  where
    list = [![1,2,3] + 1 :: Int]

-- insideCase :: Test
-- insideCase = assertEq "b" case !getA of
--   a -> "b"
--   (!(pure (++ "_")) -> "d") -> "c"
--   c -> "d"

  -- something | 1 == 0 -> !getA ++ !getA
  -- something@"a" | something == "b" -> !getC
  --               | something == !getA -> !getB
  -- "b" | !getB == !getB -> !getC
  -- _ -> ""

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
-- Think about recursive let - what do we do if we encounter `let a = !a`?
--   probably the same thing we do when we see `case x of a -> !a`
-- Should top-level/where guards of pattern bindings be treated like guards of lets? Is is possible?
--   Including the patterns of functions, e.g. when there's a binary function with a ! in the second pattern is it possible to only execute it if the first pattern matches?

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

-- current plan: we can't use unboxed tuples or sums because we can't enable the extension.
-- ACTUALLY this is not true if we do it after typechecking, I think
-- So, we use boxed tuples, and Either.
-- But we can use a church encoding for Either instead!
-- type Either a b = forall r . (a -> r) -> (b -> r) -> r
-- left a  = \l _ -> l a
-- right b = \_ r -> r b
-- In fact, this means we could construct sum types of arbitrary arity easily, which could be useful.
-- We might also need the empty sum type
-- type Void = forall r . r
-- but that's just a straightforward generalization of the above
-- Now, for consistency, I feel like we should also use church encoded tuples, i.e.
-- type (a1,a2,...,an) = forall r . (a1 -> ab -> ... -> an -> r) -> r
-- (x1,x2,...,xn) = \f -> f x1 x2 ... xn

-- So, to convert
--
--   case scrut of
--     p1 -> e1
--     p2 | g1 -> e2
--        | g2 -> e3
--     ...
--     pn -> em
--
-- into
--
--   binds <- case scrut of
--     p1 -> sum1 binds_of_e1
--     p2 | g1 -> sum2 binds_of_e2
--        | g2 -> sum3 binds_of_e3
--     ...
--     pn -> sumn binds_of_en
--   binds e1lam e2lam e3lam ... enlam
--
-- We need to perform the following steps:
-- - gather the !'d expressions in each branch
-- - construct a case expression with them, each branch's binds combined into a tuple and the branches combined into a sum
-- - call the church encoded binds tuple with one lambda expression per branch, where each !'d expression has been replaced by a lambda parameter

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
-- Parallel list/monad comps are handled such that first all the parallel statements are done, and then this is treated as a regular in series statement with the last statement, including any s in the last statement.

-- NEW PLAN:
-- Didived this whole thing into two phases (that also means two traversals, slower, but worth it):
-- parsedResultAction: replace all holes by !<expr>
--   where ! is a new function defined in that module
--     (!) :: forall m a . m a -> a
--     (!) = (!)
--   Due to collisions we probably can't actually name it that... but we can prefix it with a non-breaking space. Should just print ?! when unicode isn't supported.
-- typecheckedResultAction: Do the actual insertion of bind statements into the `binds` field, which houses the AST.
-- remember to turn on -dcore-lint for the test, since our changes now bypass the typechecker.

-- This has the major advantage of showing the user their own code in error messages, which also means we don't need to be as concerned about keeping code as close to what the user entered as possible.

-- BUT, major problem: How do we prevent something like
--   config :: String
--   config = id !getLine
-- from typechecking?

-- usually that would be converted to
--   config :: String
--   config = do
--     <!getLine> <- getLine
--     id <!getLine>
-- which fails because IO does not match [].

-- However, we might be able to do our own typechecking, in typecheckedResultAction.
-- This shouldn't even be hard. We know where we have to insert a do (or which existing do to use), we just have to make sure that any expression we use in a bind statement uses the same functor

-- I'm not sure if we can use GHC's ApplicativeDo handling with this approach.

-- However, for the last two paragraphs: We could actually just manually call rnExpr and tcExpr on our generated Do expressions, solving both problems elegantly. The already typechecked expression we're wrapping should
-- be replaced by a hole, and then we can see if the type of the hole is unifiable with the type of the wrapped expression.

-- TODO: test whether linear types work. If not, so be it, though it's probably possible to make them work.

-- Here's an interesting point: We're treating functions/lambdas differently than everything else, but what about
-- things that don't look like functions but take a constraint dict?

-- I would prefer if we could treat them the same way as non-functions, but I don't think it's possible. Consider:
--   printAndReturn :: Show a => a -> IO a
--   printAndReturn x = print x >> return x
--
--   -- -XNoMonoLocalBinds, and this is a local binding
--   let x = !(printAndReturn 4)

-- Can printAndReturn be run outside of this let binding, resulting in x :: Num a => a?
-- No, since we need to know how we print it before that, e.g. should it be "4" or "4.0"?

-- On the whole though this is not too bad, since haskell programmers are by and large familiar with this sort of thing being treated differently.
-- Also I think something that we can only do if we treat parsing and typechecking separately, which just keeps having more advantages.



-- Pros and cons for using parser plugin vs typechecker plugin:

-- Parser Con
-------------
-- if there's no constraint in the type sig, we don't know whether a let binding is polymorphic
--   user workaround: add type sig
-- Error messages look (potentially much) worse, which also makes us worry more about keeping things as close to source as possible

-- Typecheck Con
----------------
-- ApplicativeDo gets much more complicated
-- You need additional type sigs sometimes because the monad constraint is ambiguous or you only have an Applicative constraint
-- Linear types get much harder to support
-- we have to do the whole renaming and typechecking step
-- we have to do two traversals
-- we have to rely on -dcore-lint to catch any errors

-- What would happen if we do parser and just don't worry about polymorphic let? To take the previouss example:
--   let x = !(printAndReturn 4) in print (x :: Int) >> print (x :: Double)
-- would become
--   do <!(printAndReturn 4)> <- printAndReturn 4
--      let x = <!(printAndReturn 4)> in print (x :: Int) >> print (x :: Double)

-- GHCi complains about not being able to match Double with Int, so I suppose this would effectively just monomorphize the binding?
-- Maybe we actually *can* just ignore this :thinking_face:
