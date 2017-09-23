{-

The definition of a span. This also includes a BucketTree that can
put a value in a certain span.
-}
module Hledger.Data.Span
  ( Span (..)
  , contains
  , order
  -- * Bucket Tree
  , BucketTree (..)
  , lookup
  , fromSeperators
  , spans
  ) where

import Prelude hiding (lookup)

-- | A span between two a's, lower is inclusive and upper is exclusive.
data Span a =
  Span { lower :: Maybe a, upper :: Maybe a}
  deriving (Eq, Ord, Show)

-- | Check a span for containment
contains :: Ord a => Span a -> a -> Bool
contains s a = s `order` a == EQ

-- | Orders a element compared to a span
order :: (Ord a) => Span a -> a -> Ordering
order (Span l u) a
  | maybe False (a <) l = LT
  | maybe False (a >=) u = GT
  | otherwise = EQ


data BucketTree a
  = Branch { before :: BucketTree a, value :: a, after :: BucketTree a }
  | Leaf
  deriving (Eq, Show)

-- | Looks up a span in a BucketTree.
lookup :: Ord a => a -> BucketTree a -> Span a
lookup a = go (Span Nothing Nothing)
  where
    go span Leaf = span
    go span (Branch lf v rt) =
      if a < v then
        go (Span (lower span) (Just v)) lf
      else
        go (Span (Just v) (upper span)) rt

-- | Get the spans from a 'BucketTree' within a span
spans :: Ord a => Span a -> BucketTree a -> [ Span a ]
spans span bt = go span bt []
  where
    go span Leaf = (span :)
    go span (Branch lf v rt) =
      case (span `order` v) of
        LT -> go span lf
        EQ ->
          go (Span (lower span) (Just v)) lf
          . go (Span (Just v) (upper span)) rt
        GT -> go span rt


-- | Create a bucket tree from a accenting list of ordered items.
fromSeperators :: Ord a => [ a ] -> BucketTree a
fromSeperators = balanceTree . go
  where
    go (a:as) = Branch Leaf a $ go as
    go [] = Leaf

-- | balance a Tree, currently not implemented correctly
-- TODO: Write correct balance function.
balanceTree :: BucketTree a -> BucketTree a
balanceTree = id

