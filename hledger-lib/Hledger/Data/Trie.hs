{-|
Module      : HLedger.Data.Trie

A Data structure for storing information related to sequences.
-}
module Hledger.Data.Trie
  ( Trie (..)
  , singleton
  , unionWith
  , reduce
  , foldMapWithKey
  , pairs
  , lookup
  ) where

import Prelude hiding (lookup)
import Data.Monoid
import qualified Data.Map.Strict as Map

-- | A Trie is a structure that given a sequence stores
-- a value for that sequence. It is ideal for storing groupings with
-- sums or for quick searches.
data Trie a b
  = Trie
    { subtries :: !(Map.Map a (Trie a b))
    , value :: !b
    }
  deriving (Show, Eq)

instance (Monoid b, Ord a) => Monoid (Trie a b) where
  mempty = Trie mempty mempty
  mappend = unionWith mappend

-- | Create a singleton in the Trie. The value will be added
-- to all levels of the key.
singleton :: [a] -> b -> Trie a b
singleton (a:as) b =
  Trie (Map.singleton a (singleton as b)) b
singleton [] b =
  Trie Map.empty b

-- | Take the union of two 'Trie's using an operator
unionWith :: Ord a => (b -> b -> b) -> Trie a b -> Trie a b -> Trie a b
unionWith fn m1 m2 =
  Trie
    { subtries = Map.unionWith (unionWith fn) (subtries m1) (subtries m2)
    , value = fn (value m1) (value m2)
    }

-- | Lookup a value in a tree, using early termination. The only monoid
--
lookup :: (Ord a) => [a] -> Trie a b -> Maybe b
lookup [] t = Just $ value t
lookup (a:as) t = do
  t' <- Map.lookup a $ subtries t
  lookup as t'

-- | reduces a Trie
reduce
  :: (b -> [(a, c)] -> c)
  -> Trie a b
  -> c
reduce fn (Trie st v) =
  fn v . map (fmap (reduce fn)) $ Map.toAscList st

-- | Fold over a Trie with the key, collects everything in monoid m.
foldMapWithKey
  :: (Monoid m)
  => ([a] -> b -> m)
  -> Trie a b
  -> m
foldMapWithKey fn = go []
  where
    go k (Trie st v) =
      (fn k v) <> Map.foldMapWithKey (\pk a -> go (k ++ [pk]) a) st

-- | Collect all pairs in a Trie
pairs :: Trie a b -> [([a], b)]
pairs =
  flip appEndo []
  . foldMapWithKey (\k v -> Endo ((k, v) :))
