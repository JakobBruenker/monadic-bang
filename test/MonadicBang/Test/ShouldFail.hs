{-# LANGUAGE LambdaCase #-}

module MonadicBang.Test.ShouldFail (shouldFail) where

import MonadicBang.Test.Utils
import MonadicBang.Error

import GHC.Types.Name.Occurrence

import GHC.Parser.Errors.Types

shouldFail :: Test
shouldFail = do
  combined
  foo

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
\  let y = let x = print 24 in !x\n\
\  let f (a, b) = !(f a) + !(let c = c + b in c + b + z)\n\
\  pure y)\n\
\"

foo :: Test
foo = assertParseFailWith (mkErrors [S "a", O, O]) "\
\main = !getA\n\
\g = do let a = x in !a\n\
\       pure ()\n\
\f = !getB\n\
\"
