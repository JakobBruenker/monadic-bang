{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

module MonadicBang.Internal.Effect.Uniques where

import Control.Algebra
import Control.Carrier.State.Strict
import Control.Monad.IO.Class
import Data.Functor
import Data.Tuple

import GHC.Types.Unique
import GHC.Types.Unique.Supply

-- | Uniques provides arbitrarily many unique GHC Uniques
data Uniques m a where
  FreshUnique :: Uniques m Unique

freshUnique :: Has Uniques sig m => m Unique
freshUnique = send FreshUnique

newtype UniquesC m a = UniquesC {getUniquesState :: StateC UniqSupply m a}
  deriving newtype (Functor, Applicative, Monad)

-- | The "mask" (Char) supplied is purely cosmetic, making it easier to figure out where a Unique was born.
--
-- See Note [Uniques for wired-in prelude things and known masks] in GHC.Builtin.Uniques
runUniquesIO :: MonadIO m => Char -> UniquesC m a -> m a
runUniquesIO mask (UniquesC s) = flip evalState s =<< liftIO (mkSplitUniqSupply mask)

runUniques :: Functor m => UniqSupply -> UniquesC m a -> m a
runUniques uniqSupply (UniquesC s) = evalState uniqSupply s

instance Algebra sig m => Algebra (Uniques :+: sig) (UniquesC m) where
  alg hdl sig ctx = case sig of
    L FreshUnique -> UniquesC . state $ fmap (ctx $>) . swap . takeUniqFromSupply
    R other -> UniquesC (alg ((.getUniquesState) . hdl) (R other) ctx)
  {-# INLINE alg #-} 
