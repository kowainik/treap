{- | Pure efficient implementation of the @treap@ data structure.

__NOTE:__ Letter \( d \) in the documentation means depth of the tree.
-}

{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies      #-}

module Treap.Pure
       ( -- * Data structure
         Treap (..)

         -- * Smart constructors
       , empty
       , one

         -- * Core functions
       , split
       , merge
       ) where

import GHC.Exts (IsList (..))
import GHC.Generics (Generic)

----------------------------------------------------------------------------
-- Data structure and instances
----------------------------------------------------------------------------

-- | 'Treap' data structure.
data Treap k p a
    = Node !k !p !a !(Treap k p a) !(Treap k p a)
    | Empty
    deriving (Show, Read, Generic, Functor, Foldable, Traversable)

instance IsList (Treap k p a) where
    type Item (Treap k p a) = (k, p, a)

    fromList :: [(k, p, a)] -> Treap k p a
    fromList = error "Not implemented!"

    toList :: Treap k p a -> [(k, p, a)]
    toList = error "Not implemented!"

----------------------------------------------------------------------------
-- Smart constructors
----------------------------------------------------------------------------

-- | \( O(1) \). Create empty 'Treap'.
empty :: Treap k p a
empty = Empty

-- | \( O(1) \). Create singleton 'Treap'.
one :: k -> p -> a -> Treap k p a
one k p a = Node k p a Empty Empty

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
