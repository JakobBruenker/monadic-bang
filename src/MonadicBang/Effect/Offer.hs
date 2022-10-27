{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

module MonadicBang.Effect.Offer where

import Control.Algebra
import Control.Carrier.State.Strict
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M

-- | Offers a number of things that can be yoinked, but only once
data Offer k v m a where
  Yoink :: k -> Offer k v m (Maybe v)

yoink :: Has (Offer k v) sig m => k -> m (Maybe v)
yoink = send . Yoink

newtype OfferC k v m a = OfferC {getOfferState :: StateC (Map k v) m a}
  deriving newtype (Functor, Applicative, Monad)

-- Returns the result of the computation, along with the remaining offers
runOffer :: Map k v -> OfferC k v m a -> m (Map k v, a)
runOffer o (OfferC s) = runState o s

instance (Algebra sig m, Ord k) => Algebra (Offer k v :+: sig) (OfferC k v m) where
  alg hdl sig ctx = case sig of
    L (Yoink k) -> OfferC do
      (mv, remaining) <- M.updateLookupWithKey (\_ _ -> Nothing) k <$> get
      put remaining
      pure (mv <$ ctx)
    R other -> OfferC (alg ((.getOfferState) . hdl) (R other) ctx)
