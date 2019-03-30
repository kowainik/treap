{- | Pure efficient implementation of the @treap@ data structure.

__NOTE:__ Letter \( d \) in the documentation means depth of the tree.
-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}

module Treap.Pure
       ( -- * Data structure
         Treap (..)

         -- * Smart constructors
       , empty
       , one

         -- * Core functions
       , split
       , merge

         -- * Interface functions
       , lookup
       , insert
       , delete
       ) where

import Prelude hiding (lookup)

import Control.DeepSeq (NFData)
import Data.Foldable (foldl')
import GHC.Exts (IsList (..))
import GHC.Generics (Generic)

----------------------------------------------------------------------------
-- Data structure and instances
----------------------------------------------------------------------------

-- | 'Treap' data structure.
data Treap k p a
    = Node !k !p !a !(Treap k p a) !(Treap k p a)
    | Empty
    deriving (Show, Read, Eq, Generic, Functor, Foldable, Traversable)
    deriving anyclass (NFData)

{- | Typeclass that allows to create 'Treap' from the list of triples. If all
priorities are random then the expected performance of the @fromList@ function
is \( O(n\ \log \ n)\).
-}
instance (Ord k, Ord p) => IsList (Treap k p a) where
    type Item (Treap k p a) = (k, p, a)

    fromList :: [(k, p, a)] -> Treap k p a
    fromList = foldl' (\t (k, p, a) -> insert k p a t) Empty

    -- TODO: make more efficient
    toList :: Treap k p a -> [(k, p, a)]
    toList Empty            = []
    toList (Node k p a l r) = toList l ++ (k, p, a) : toList r

----------------------------------------------------------------------------
-- Smart constructors
----------------------------------------------------------------------------

-- | \( O(1) \). Create empty 'Treap'.
empty :: Treap k p a
empty = Empty
{-# INLINE empty #-}

-- | \( O(1) \). Create singleton 'Treap'.
one :: k -> p -> a -> Treap k p a
one k p a = Node k p a Empty Empty
{-# INLINE one #-}

----------------------------------------------------------------------------
-- Core functions
----------------------------------------------------------------------------

{- | \( O(d) \). Split 'Treap' by the given key. @split k t@ returns such pair of treaps
@(t1, t2)@ that:

1. Tree @t1@ contains all keys \( < k \).
2. Tree @t2@ contains rest of the keys.
-}
split :: Ord k => k -> Treap k p a -> (Treap k p a, Treap k p a)
split k = \case
    Empty -> (Empty, Empty)
    Node tk tp ta left right
      | tk < k ->
          let (!newRight, !t2) = split k right
          in (Node tk tp ta left newRight, t2)
      | otherwise ->
          let (!t1, !newLeft) = split k left
          in (t1, Node tk tp ta newLeft right)

{- | \( O(\max\ d_1\ d_2) \). Merge two 'Treap's into single one. This function
assumes that all keys in the first 'Treap' should be less than all keys in the
second 'Treap'.

>>> putStrLn $ pretty @Int @Int @Int $ merge (merge (one 1 2 3) (one 3 4 5)) (merge (one 4 10 0) (one 5 3 1))
   4,10:0
     ╱╲
    ╱  ╲
   ╱    ╲
 3,4:5 5,3:1
  ╱
1,2:3
-}
merge :: Ord p => Treap k p a -> Treap k p a -> Treap k p a
merge Empty r = r
merge l Empty = l
merge l@(Node k1 p1 a1 l1 r1) r@(Node k2 p2 a2 l2 r2)
    | p1 > p2   = Node k1 p1 a1 l1 (merge r1 r)
    | otherwise = Node k2 p2 a2 (merge l l2) r2

----------------------------------------------------------------------------
-- Core functions
----------------------------------------------------------------------------

-- | \( O(d) \). Lookup a value by a given key inside 'Treap'.
lookup :: forall k p a . Ord k => k -> Treap k p a -> Maybe a
lookup k = go
  where
    go :: Treap k p a -> Maybe a
    go Empty = Nothing
    go (Node tk _ ta l r) = case compare k tk of
        EQ -> Just ta
        LT -> go l
        GT -> go r

-- | \( O(d) \). Insert a value into 'Treap' by given key and priority.
insert :: forall k p a . (Ord k, Ord p) => k -> p -> a -> Treap k p a -> Treap k p a
insert k p a = go
  where
    go :: Treap k p a -> Treap k p a
    go Empty = one k p a
    go t@(Node tk tp ta l r)
      | p <= tp = case compare k tk of
          EQ -> t
          LT -> Node tk tp ta (go l) r
          GT -> Node tk tp ta l (go r)
      | otherwise =
          let (!newL, !newR) = split k t
          in Node k p a newL newR

{- | \( O(d) \). Delete 'Treap' node that contains given key. If there is no
such key, 'Treap' remains unchanged.
-}
delete :: forall k p a . (Ord k, Ord p) => k -> Treap k p a -> Treap k p a
delete k = go
  where
    go :: Treap k p a -> Treap k p a
    go Empty = Empty
    go (Node tk tp ta l r) = case compare k tk of
        EQ -> merge l r
        LT -> Node tk tp ta (go l) r
        GT -> Node tk tp ta l (go r)
