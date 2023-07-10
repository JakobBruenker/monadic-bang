{-# LANGUAGE LambdaCase #-}

module MonadicBang.Test.ShouldFail (shouldFail) where

import MonadicBang.Test.Utils
import MonadicBang.Error

import GHC.Types.Name.Occurrence

import GHC.Parser.Errors.Types

shouldFail :: Test
shouldFail = do
  combined
  various
  letStmt
  letInLet

data ErrorData
  = S String -- ^ Out of scope variable
  | O        -- ^ Bang outside of do

mkErrors :: [ErrorData] -> [PsMessage]
mkErrors = map (customError . toError)
  where
    toError = \cases
      (S var) -> ErrOutOfScopeVariable $ mkVarOcc var
      O -> ErrBangOutsideOfDo

combined :: Test
combined = assertParseFailWith (mkErrors [S "x", S "f", S "a", S "b", S "b", O, O]) "\
\!(!do\n\
\  x <- getA\n\
\  let y = let x = print 24 in x\n\
\  let f (a, b) = !(f a) + !(let c = c + b in c + b + z)\n\
\  pure y)\n\
\"

various :: Test
various = assertParseFailWith (mkErrors [S "a", S "x", S "y", S "b", S "b1", S "a2", O, O, O]) "\
\main = !getA\n\
\g = do let a = x in !a\n\
\       pure ()\n\
\f = !getB\n\
\h = \\x -> !x\n\
\i = do \\y -> !y\n\
\j = let z = z in do \\_ -> !z -- no error\n\
\k = case () of a -> do case () of b -> !(a + b)\n\
\l = let c1 = c1 in do let b1 = b1 in !(let c1 = c1 in a1 + b1 + c1)\n\
\m = do !(let a2 = a2 in !a2)\n\
\"

letStmt :: Test
letStmt = assertParseFailWith (mkErrors [S "x"]) "\
\main = do\n\
\  let x = !x\n\
\  pure x\n\
\"

letInLet :: Test
letInLet = assertParseFailWith (mkErrors [S "y", S "x"]) "\
\main = do\n\
\  let x _ = x in let y = y in !y + !x\n\
\"
