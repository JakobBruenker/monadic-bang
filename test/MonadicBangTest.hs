{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}

{-# OPTIONS -fplugin=MonadicBang -fplugin-opt=MonadicBang:-v #-}

module Main (main) where

import GHC.Stack
import Data.Char

getA, getB, getC :: IO String
getA = pure "a"
getB = pure "b"
getC = pure "c"

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
  multiWayIf
  guards
  viewPat
  insideWhere
  insideCase

assertEq :: (HasCallStack, Show a, Eq a) => a -> a -> IO ()
assertEq expected actual
  | expected == actual = pure ()
  | otherwise = withFrozenCallStack do
      error $ "Expected " <> show expected <> ", but got " <> show actual

type Test = HasCallStack => IO ()

withoutDo :: Test
withoutDo = do assertEq "a" !getA

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
-- Fun fact: 
-- This:
-- insideCase = do assertEq "b"
--   case !getA of
-- wouldn't work because it's parsed as `(do assertEq "b") case !getA of`
-- which would mean !getA is not inside a do-block
insideCase = do
  assertEq "b"
    case !getA of
      (!(pure (++ "_")) -> "d") -> c ++ s123
        where c = !getC
              s123 = do pure !"123"
      "c" -> "d"
      _a -> "b"

multiWayIf :: Test
multiWayIf = do
  assertEq "b" if
    | !getA == !getA -> !getB
    | otherwise      -> !getC

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
--   Due to collisions we probably can't actually name it that... (maybe add a space?) though we might be able to get away with providing an exact name?
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
-- Update: Typecheck plugin has worse problems, see corresponding branch

-- Santiweight made an interesting suggestion here:
-- https://github.com/ghc-proposals/ghc-proposals/issues/527#issuecomment-1261911356
-- Don't ever insert `do`s automatically, always rely on the user to do it.
-- At first I didn't like this, because it makes some simple things slightly more verbose.
-- But the more I think about it, especially with let and recursive bindings in both let and case, it makes the specification so much simpler
-- Additionally, a lot of use-cases would be inside a do-block anyway,

-- You could go with a rule like "Don't introduce a do-block anywhere except at the top-level of a function definition", buut maybe it makes sense to start simple.
-- That would be a backwards-compatible change, anyway... mostly. Not entirely, since it means we also don't insert a new do for let-bindings that take arguments.

-- Quick note on where:
-- On the top-level, obviously you have no choice but to treat it as an independent block, since there can be no `do` that expressions can be evac'd in.
-- But in local definitions, you could think about evacing to the `do` that the local definition is inside of. This is also true for where in `case`.
-- I'm not sure I like the idea, but it's something to consider. I'm not sure that I dislike the idea, either.

-- The obvious problem would be that there could be variables in !'d expressions that aren't valid outside of the where clause, but the same can happen with recursive let or let with arguments.
-- There are two solutions to this:
-- 1. Keep track of the variables that are in scope, and raise an error if the user tries to do something bad
-- Pro: Better error messages
-- Con: Adds a decent amount of complexity
-- -> Hacky solution: At each level of the AST, replace variable names with themselves, plus one " \b" per level (space + backspace).
-- This should work, but maybe has unintented side effects? We'd have to see.
-- We might also be able to use Exact RdrNames, which would be a less hacky
-- However, I'm not sure this actually makes it any easier... We still have to keep track of which names are brought in scope where so we know how many levels to add
-- 2. Just let GHC throw errors
-- BIG con: what happens if a variable is shadowed, does the ! just evac the outer one instead? Is there some way to avoid that?
-- It might be worth it to just accept that for the time being... XXX JB keep that in mind when writing docs
-- Maayyybe we could add a renamer plugin, that could go through and check these things after the fact. Of course that would require a second pass
-- (maybe allow user to turn it off via command line option)
-- Programming that also sounds fairly annoying though...
-- XXX JB anyway, add a test for the above case, i.e. where you try to use variables that are not in scope
-- (it's not obvious how to do that, since we currently don't have any tests that expect compiler errors - maybe you can start a ghc session inside of this code to do it)

-- Thinking about case a bit more, a reasonable compromise might be:
-- - We have the special behavior for any expression in the rhs of case
-- - We don't have special behavior for the lhs of case
-- That would simplify things a lot, actually, while still offering, like, 90% of the advantages.
-- The same applies to MultiWayIf.

-- Another quick note on if/case:
-- Usually, the fancy thing where you evac it to the do and only do actions that are needed should be fine.
-- But what if the scrutinee uses variables that are out of scope there?
-- I suppose it's not any different from e.g. let. The question to consider though is if there are situations where the idris-like behavior would be more intuitive.
-- e.g.

{-
do putStrLn "hi"
   let doSomething arg1 arg2 =
         if arg1 == arg2
           then !fetchUser
           else !fetchGuest ++ " (Guest)"
   let x = doSomething user1 user2
-}

-- I think the fancy behavior could still be fine here.
-- How would you fix it, as a user?
-- You would need to place a `do` inside the let, like

{-
do putStrLn "hi"
   let doSomething arg1 arg2 = do
         if arg1 == arg2
           then !fetchUser
           else !fetchGuest ++ " (Guest)"
   let x = doSomething user1 user2
-}

-- You might think. That doesn't actually work though, since doSomething would now have to be a monadic action for this to work, but it returns a non-monadic action, so you would have to change more...

{-
do putStrLn "hi"
   let doSomething arg1 arg2 = do
         if arg1 == arg2
           then pure !fetchUser
           else pure $ !fetchGuest ++ " (Guest)"
   let x = !(doSomething user1 user2)
-}

-- Now let's compare this with the solution you would need with the Idris way:

{-
do putStrLn "hi"
   let doSomething arg1 arg2 =
         if arg1 == arg2
           then do pure !fetchUser                   -- of course this is just fetchUser
           else do pure $ !fetchGuest ++ " (Guest)"
   let x = !(doSomething user1 user2)
-}

-- Not much of a difference there, and honestly, not too difficult to implement as a user. I conclude that doing it the Idris way is probably better... For now.

-- One interesting question is, how does this compare with e.g. python?

{-
print("hi")
def doSomething(arg1, arg2):
  if arg1 == arg2:
    return fetchUser
  else:
    return fetchGuest ++ " (Guest)"
x = doSomething user1 user2
-}

-- I'm not gonna lie, that's a bit nicer than juggling pures around... But maybe that's okay.
-- I kind of wish we had a shorter way to write "pure" as well, but I'm not doing that. 5 characters (including space) isn't that bad.
