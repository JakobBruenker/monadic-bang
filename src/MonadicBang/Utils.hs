module MonadicBang.Utils where

import Data.Monoid
import Control.Monad.Trans.Maybe

type DList a = Endo [a]

fromDList :: DList a -> [a]
fromDList = appEndo ?? []

(??) :: Functor f => f (a -> b) -> a -> f b
fs ?? x = ($ x) <$> fs

panic :: String -> a
panic message = error $ unlines ["MonadicBang panic:", message, "", submitReport]
  where
    submitReport = "This is likely a bug. Please submit a bug report under https://github.com/JakobBruenker/monadic-bang/issues"

-- This is included in transformers 0.6, but that can't be used together with ghc 9.4
hoistMaybe :: Applicative m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure
