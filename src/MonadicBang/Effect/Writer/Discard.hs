{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}

-- | A Writer carrier that discards any values it is told
module MonadicBang.Effect.Writer.Discard where

import Control.Algebra
import Control.Effect.Writer

newtype DiscardC w m a = DiscardC { evalDiscardC :: m a }
  deriving newtype (Functor, Applicative, Monad)

evalWriter :: (Monoid w, Algebra sig m) => DiscardC w m a -> m a
evalWriter = evalDiscardC

instance (Monoid w, Algebra sig m) => Algebra (Writer w :+: sig) (DiscardC w m) where
  alg hdl sig ctx = DiscardC $ case sig of
    L writer -> case writer of
      Tell _ -> pure ctx
      Listen m -> fmap (mempty,) <$> evalWriter (hdl (m <$ ctx))
      Censor _ m -> evalWriter (hdl (m <$ ctx))
    R other -> alg (evalDiscardC . hdl) other ctx
