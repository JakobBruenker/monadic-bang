{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE CPP #-}

module MonadicBang.Internal.Utils where

import Control.Monad.Trans.Maybe
import Data.Monoid
import GHC.Stack (withFrozenCallStack, HasCallStack)

type DList a = Endo [a]

-- | Handle a specific AST node
type Handler m a = a -> m a

-- | Try handling an AST node, but may fail (usually because the handler is not
-- applicable)
type Try m a = Handler (MaybeT m) a

{-# INLINE fromDList #-}
fromDList :: DList a -> [a]
fromDList = appEndo ?? []

{-# INLINE (??) #-}
(??) :: Functor f => f (a -> b) -> a -> f b
fs ?? x = ($ x) <$> fs

#if MIN_VERSION_transformers(0,6,0)
#else
{-# INLINE hoistMaybe #-}
hoistMaybe :: Applicative m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure
#endif

panic :: HasCallStack => String -> a
panic message = withFrozenCallStack $ error $
  unlines ["MonadicBang panic:", message, "", submitReport]
  where
    submitReport = "This is likely a bug. Please submit a bug report at https://github.com/JakobBruenker/monadic-bang/issues"
