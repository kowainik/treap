{- | Pure efficient implementation of the @treap@ data structure.

__NOTE:__ Letter \( d \) in the documentation means depth of the tree.
-}

module Treap.Pure
       ( -- * Data structure
         Treap (..)

         -- * Core functions
       , split
       ) where

-- | 'Treap' data structure.
data Treap k p a
    = Node !k !p !a !(Treap k p a) !(Treap k p a)
    | Empty
    deriving (Show, Read)

{- | \( O(d) \). Split 'Treap' by the given key. @split k t@ returns such pair of treaps
@(t1, t2)@ that:

1. Tree @t1@ contains all keys \( < k \).
2. Tree @t2@ contains rest of the keys.

@
   x
  ╱ ╲
 ╱   ╲
y     z
@
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
