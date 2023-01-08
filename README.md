# Monadic Bang

This is a GHC Parser plugin for GHC 9.4 and above, intended to make monadic code within `do`-blocks more concise and nicer to work with. Works with HLS.

This is heavily inspired by [Idris's !-notation](https://idris2.readthedocs.io/en/latest/tutorial/interfaces.html#notation), but with some [important differences](#comparison-with-idriss--notation).

## Contents

1. [Motivating Examples](#motivating-examples)
2. [Usage](#usage)
3. [Cute Things](#cute-things)
4. [Caveats](#caveats)
5. [Details](#details)
6. [Comparison with Idris's `!`-notation](#comparison-with-idriss--notation)

## Motivating Examples

Let's look at a few examples where Haskell syntax can be a bit annoying when it comes to monads - and what this plugin allows you to write instead:

When you use `Reader` or `State`, you will often have to use `<-` to bind fairly simple expressions:

```haskell
launchMissile :: StateT Int IO ()
launchMissile = do
  count <- get
  liftIO . putStrLn $ "The " <> show count <> "'th missile has been launched"
  modify' (+ 1)
```

```haskell
help :: Reader Config String
help = do
  manualLink <- asks (.links.manual)
  email <- asks (.contact.email)
  pure $
    "You can find help by going to " <> manualLink <>
    " or writing us at " <> email
```

With Monadic Bang, you can instead write
```haskell
launchMissile :: StateT Int IO ()
launchMissile = do
  liftIO . putStrLn $ "The " <> show !get <> "'th missile has been launched"
  modify' (+ 1)
```

```haskell
help :: Reader Config String
help = do
  pure $
    "You can find help by going to " <> (!ask).links.manual <>
    " or writing us at " <> (!ask).contact.email
```

With IORefs, STRefs, mutable arrays, and so on, you'll often have to write code that looks like this, having to use somewhat redundant variable names:

```haskell
addIORefs :: IORef Int -> IORef Int -> IO Int
addIORefs aRef bRef = do
  a <- readIORef aRef
  b <- readIORef bRef
  pure $ a + b
```

With Monadic Bang, you can write

```haskell
addIORefs :: IORef Int -> IORef Int -> IO Int
addIORefs a b = do pure $ !(readIORef a) + !(readIORef b)
```

Implicit parameter definitions have somewhat more limited syntax than regular definitions: You can't write something like `?foo <- action`.  
That lead me to have to write this in a Vulkan program:

```haskell
initQueues = do
  let getQueue = getDeviceQueue ?device
  graphicsQueue <- getQueue ?graphicsQueueFamily 0
  presentQueue  <- getQueue ?presentQueueFamily  0
  computeQueue  <- getQueue ?computeQueueFamily  1
  let ?graphicsQueue = graphicsQueue
      ?presentQueue  = presentQueue
      ?computeQueue  = computeQueue
  pure Dict
```

with Monadic Bang, I can write

```haskell
initQueues = do
  let getQueue = getDeviceQueue ?device
  let ?graphicsQueue = !(getQueue ?graphicsQueueFamily 0)
      ?presentQueue  = !(getQueue ?presentQueueFamily  0)
      ?computeQueue  = !(getQueue ?computeQueueFamily  1)
  pure Dict
```

Take this (slightly adapted) code used for the test suite of this very plugin:

```haskell
settings :: MonadIO m => m Settings
settings = ... -- some long function body

initialDynFlags :: MonadIO m => m DynFlags
initialDynFlags = do
  settings' <- settings
  dflags <- defaultDynFlags settings' llvmConfig
  pure $ dflags{generalFlags = addCompileFlags $ generalFlags dflags}
```

With this plugin, I can instead write

```haskell
settings :: MonadIO m => m Settings
settings = ... -- some long function body

initialDynFlags :: MonadIO m => m DynFlags
initialDynFlags = do
  dflags <- defaultDynFlags !settings llvmConfig
  pure $ dflags{generalFlags = addCompileFlags $ generalFlags dflags}
```

Or, to take some more code from this plugin's implementation

```haskell
do logger <- getLogger
   liftIO $ logMsg logger MCInfo (UnhelpfulSpan UnhelpfulNoLocationInfo) m
```
Why have `logger` *and* `getLogger` when you can instead write

```haskell
do liftIO $ logMsg !getLogger MCInfo (UnhelpfulSpan UnhelpfulNoLocationInfo) m
```

The pattern you might have noticed here is that this plugin is convenient
whenever you have a `do`-block with a `<-` that doesn't do pattern matching,
whose bound variable is only used once, and has a short right-hand side.  While
that might sound like a lot of qualifiers, it does occur fairly often in
practice.

## Usage

To use this plugin, assuming you use cabal, you have to add `monadic-bang` to the `build-depends` stanza in your `.cabal` file. Then you can either add `-fplugin=MonadicBang` to the `ghc-options` stanza, or add

```haskell
{-# OPTIONS -fplugin=MonadicBang #-}
```

to the top of the files you want to use it in.

This should also allow HLS to pick up on the plugin, as long as you use HLS 1.9.0.0 or above.

The plugin supports a couple of options, which you can provide via invocations of `-fplugin-opt=MonadicBang:<option>`. The options are:

- `-ddump`: Print the altered AST
- `-preserve-errors`: Keep parse errors about `!` outside of `do` in their original form, rather then a more relevant explanation. This is mainly useful if another plugin expects those errors.

## Cute Things

### Idiom Brackets Alternative

In some cases where idiom brackets would be ideal, `!` can be a reasonable alternative. For example, compare these four options:

```haskell
1. liftA2 (&&) (readIORef useMetric) (readIORef useCelsius)
2. (&&) <$> readIORef useMetric <*> readIORef useCelsius
   -- hypothetical idiom brackets:
3. [| readIORef useMetric && readIORef useCelsius |]
   -- Monadic Bang:
4. do pure (!(readIORef useMetric) && !(readIORef useCelsius))
```

while `<$>` and `<*>` are probably better for prefix functions, `!` plays nicer with infix operators.

If you have `-XApplicativeDo` enabled, this even works with `Applicative` instances.

### Nested `!`

`!` can easily be nested. E.g. you could have

```haskell
do putStrLn !(readFile (!getArgs !! 1))
```

For how this is desugared, see [Desugaring](#desugaring)

### Using `-XQualifiedDo`

`!` always have to be used inside a `do`-block, but it *can* be a qualified `do`-block. For example, if you use `-XLinearTypes`, you could write things like

```haskell
{-# LANGUAGE QualifiedDo, BlockArguments, OverloadedStrings #-}
import Prelude.Linear
import Control.Functor.Linear as Linear
import System.IO.Resource.Linear

main :: IO ()
main = run Linear.do
  Linear.pure !(move Linear.<$> hClose !(hPutStrLn !(openFile "tmp" WriteMode) "foo"))
```

which would be desugared as

```Haskell
main :: IO ()
main = run Linear.do
  a <- openFile "tmp" WriteMode
  b <- hPutStrLn a "foo"
  c <- move Linear.<$> hClose b
  Linear.pure c
```

### List comprehensions

List comprehensions are kind of just special `do`-blocks, so `!` can be used here, as well (and also in monad comprehensions). Example:

```haskell
[ x + ![1, 2, 3] | x <- [60, 70, ![800, 900]] ]
```
This would be equivalent to
```haskell
[ x + b | a <- [800, 900], x <- [60, 70, a], b <- [1, 2, 3]]
```

The reason `b <- ...` at the end here instead of the beginning is that everything that appears to the left of the `|` in a list comprehension is essentially comparable to the last statement of a `do`-block (+ `pure`).

### Get Rid of `<-`

In principle, every instance of `pattern <- action` in a `do`-block could be replaced by `let pattern = !action`. Should they? That's a separate question, though it could be a viable style.

The implicit parameter example in the first section is a valid use case of this.

### Monadic Variants

Oftentimes, some generic function exists, but then it turns out that a monadic variant of said function would be useful as well. For example, hoogle finds at least a dozen different packages offering `whenM`. With this plugin, you can instead write

```haskell
main = do
  when (null !getArgs) $ print usage
  ...
```

⚠️ NB: This would not work for e.g. `whileM`. In implementations of `whileM`, the condition is re-evaluated after every iteration. If you wrote e.g. `while (!(readIORef i) > 0)`, it would only be evaluated once, before the first iteration.

## Caveats

There are a few disadvantages to using this that are worth mentioning:

- Since the plugin modifies the source code, the location info in error messages might look a bit strange, since it contains the desugared version. This shouldn't be an issue if you use HLS or another tool to highlight errors within your editor.
- HLint currently does not work with this plugin (HLint will show you a parse error if you try to use `!`.)
- If there are fatal parse errors in the source code, unfortunately each `!` will also be highlighted as a parse error. This is unavoidable at the moment, since the plugin can only intercept those messages if the module is otherwise successfully parsed.
- Arguably this makes `do`-desugaring slightly more confusing - e.g., compare the following:

  ```haskell
  do put 4
     put 5 >> print !get
  ``` 
  
  ```haskell
  do put 4
     put 5
     print !get
  ```  
  
  With the usual desugaring rules, whether you use `>>` or a new line shouldn't make a difference, but here, the first snippet will print `4`, while the second snippet will print `5`.

## Details

While the above information should cover most use cases, there are some details that could sometimes be relevant

### Desugaring

The desugaring is essentially what one would expect from comparing the motivating examples with the versions using `!`.

To illustrate with a fairly extensive example:

```haskell
x = g do
  foo
  bar <- !a + !(!b ++ !c)
  baz <- case !d of
    (!f -> e) -> do !g e
```

is desugared into

```haskell
x = g do
  foo
  <!a> <- a
  <!b> <- b
  <!c> <- c
  <!(!b ++ !c)> <- <!b> ++ <!c>
  bar <- <!a> + <!(!b ++ !c)>
  <!d> <- d
  <!f> <- f
  baz <- case <!d> of
    (<!f> -> e) -> do
      <!g> <- g
      <!g> e
```

where `<!a>` etc. are simply special variable names.

So, broadly speaking, the order in which things are bound is top-to-bottom (statement-wise), inside-out, and left-to-right.

This can be important when the order of effects matters - though if order does matter, `!` might not be the clearest way to express things.

`!` will only bubble up to the nearest `do`-block. To illustrate:

```haskell
x = do when nuclearStrikeDetected $ log !launchMissiles

y = do when nuclearStrikeDetected $ do log !launchMissiles
```

`x` will launch the missiles regardless of whether or not a strike has been detected. But it will only log the results in the case of detection.
`y` will only launch the missiles (and log the results) if a strike has been detected.

The desugaring:

```haskell
x = do
  <!launchMissiles> <- launchMissiles
  when nuclearStrikeDetected $ log <!launchMissiles>

y = do
  when nuclearStrikeDetected $ do
    <!launchMissiles> <- launchMissiles
    log <!launchMissiles>
```

The story for `case` and `if` expressions is similar, `!` in the individual branches will *all* be executed unless the branches have their own `do`-blocks.

### Variable scope

A variable can be used inside a `!` if
- it was bound outside the current `do`-block
- or it was bound before the statement the `!` is in
- or it is bound inside the `!`

In other words, this is legal:
```haskell
f x = do
  let a = a
  foo !(let b = b in x + a + b)
```
but this is not:
```haskell
c = do
  let a = a in foo !a
```
That's because this would be desugared as
```haskell
c = do
  <!a> <- a
  let a = a in foo <!a>
```
but `a` is not in scope in the second line.

### Where it can be used

It can be used in any expression that is somewhere inside a `do`-block. In particular, this includes for example `where`-blocks in `case`-expressions:

```haskell
main = do
  putStrLn case !getLine of
    "print args" -> prettyArgs "\n"
      where prettyArgs sep = intercalate sep !getArgs
    "greeting" -> "hello there!"
```

and view patterns

```haskell
do (extract !getSettings -> contents) <- readArchive
   print contents
```

## Comparison with Idris's `!`-notation

The main difference is that Idris will insert a `do` if there is none - e.g. this is legal in Idris:

```haskell
f : IO ()
f = putStrLn !getLine
```

but (assuming it's at top-level) wouldn't be with this plugin; you would have to write `f = do putStrLn !getLine` instead.

Some other differences:
- In Idris, `!`'d expressions cannot escape outside of a lambda expression (it effectively inserts a new `do` at the beginning of the lambda body instead)
- The same difference applies to `let` bindings that define functions
