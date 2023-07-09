{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module MonadicBang.Test.Utils.QualifiedDo where

import Prelude (Int, (+), const)

(>>=) :: Int -> (Int -> Int) -> Int
a >>= f = a + f a

(>>) :: Int -> Int -> Int
a >> b = a >>= const b
