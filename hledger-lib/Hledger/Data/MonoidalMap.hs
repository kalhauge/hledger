
module Hledger.Data.MonoidalMap
  ( MonoidalMap (..)
  , singleton
  ) where

import qualified Data.Map as Map

-- Monoidal Map

newtype MonoidalMap a b = MonoidalMap { unMonoidalMap :: Map.Map a b }
  deriving (Show, Eq)

instance (Monoid b, Ord a) => Monoid (MonoidalMap a b) where
  mempty = MonoidalMap mempty
  mappend m1 m2 = MonoidalMap $
    Map.unionWith mappend (unMonoidalMap m1) (unMonoidalMap m2)

singleton :: a -> b -> MonoidalMap a b
singleton a b = MonoidalMap $ Map.singleton a b
