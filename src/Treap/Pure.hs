{- | Pure efficient implementation of the implicit treap data structure with the
segment tree interface.

__NOTE:__ Letter \( d \) in the documentation below means depth of the tree. Real
depth depends on the strategy for creating 'Priority'. If the strategy is poor,
the depth can be linear. However, if priorities are generated randomly, expected
depth is \( O(\log \ n) \).
-}

{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Treap.Pure
       ( -- * Data structure
         Size (..)
       , Priority (..)
       , Treap (..)

         -- * Smart constructors
       , empty
       , one

         -- * Query functions
       , size
       , sizeInt
       , monoid
       , at
       , query

         -- * Cuts and joins
       , splitAt
       , merge
       , take
       , drop
       , rotate

         -- * Modification functions
       , insert
       , delete

         -- * Core internal functions
       , recalculate
       ) where

import Prelude hiding (drop, lookup, splitAt, take)

import Control.DeepSeq (NFData)
import Data.Foldable (foldl')
import Data.Word (Word64)
import GHC.Exts (IsList (..))
import GHC.Generics (Generic)

import Treap.Measured (Measured (..))

----------------------------------------------------------------------------
-- Data structure and instances
----------------------------------------------------------------------------

{- | Size of the 'Treap' data structure.
-}
newtype Size = Size
    { unSize :: Int
    } deriving stock (Show, Read, Generic)
      deriving newtype (Eq, Ord, Num, NFData)

{- | Priority in the 'Treap' data structure.
-}
newtype Priority = Priority
    { unPriority :: Word64
    } deriving stock (Show, Read, Generic)
      deriving newtype (Eq, Ord, NFData)

-- | 'Treap' data structure.
data Treap m a
    = Node !Size !Priority !m a !(Treap m a) !(Treap m a)
    | Empty
    deriving (Show, Read, Eq, Generic)
    deriving anyclass (NFData)

-- | \( O(1) \). Takes cached value from the root.
instance Monoid m => Measured m (Treap m a) where
    measure = monoid
    {-# INLINE measure #-}

-- #if __GLASGOW_HASKELL__ >= 806
-- -- | Safe 'Functor' instance that performs recalculation of monoidal accumulator.
-- instance (forall b . Measured m b) => Functor (Treap m) where
--     fmap :: forall a b . (a -> b) -> Treap m a -> Treap m b
--     fmap f = go
--       where
--         go :: Treap m a -> Treap m b
--         go Empty = Empty
--         go (Node _ p _ a l r) = recalculate $ new p (f a) (go l) (go r)
-- #endif

{- | This instance allows to create 'Treap' from the list of triples. If all
priorities are random then the expected performance of the @fromList@ function
is \( O(n\ \log \ n)\).

__TODO:__ It's possible to implement \( O(n) \) algorithm however.
-}
instance Measured m a => IsList (Treap m a) where
    type Item (Treap m a) = (Priority, a)

    -- TODO: implement O(n) algorithm
    fromList :: [(Priority, a)] -> Treap m a
    fromList =
        foldl' (\t (i, p, a) -> insert i p a t) Empty
        . zipWith (\i (p, a) -> (i, p, a)) [0..]

    -- TODO: make more efficient
    toList :: Treap m a -> [(Priority, a)]
    toList Empty              = []
    toList (Node _ p _ a l r) = toList l ++ (p, a) : toList r

----------------------------------------------------------------------------
-- Smart constructors
----------------------------------------------------------------------------

-- | \( O(1) \). Creates empty 'Treap'.
empty :: Treap m a
empty = Empty
{-# INLINE empty #-}

-- | \( O(1) \). Creates singleton 'Treap'.
one :: Measured m a => Priority -> a -> Treap m a
one p a = Node (Size 1) p (measure a) a Empty Empty
{-# INLINE one #-}

----------------------------------------------------------------------------
-- Query functions
----------------------------------------------------------------------------

{- | \( O(1) \). Returns the number of the elements in the 'Treap'. Always
non-negative.

__Properties:__

* \( \forall (t\ ::\ \mathrm{Treap}\ m\ a)\ .\ \mathrm{size}\ t \geqslant 0 \)
-}
size :: Treap m a -> Size
size = \case
    Empty -> Size 0
    Node s _ _ _ _ _ -> s
{-# INLINE size #-}

-- Convenient internal function
sizeInt :: Treap m a -> Int
sizeInt = unSize . size
{-# INLINE sizeInt #-}

-- | \( O(1) \). Returns accumulated value in the root of the tree.
monoid :: Monoid m => Treap m a -> m
monoid = \case
    Empty -> mempty
    Node _ _ m _ _ _ -> m
{-# INLINE monoid #-}

-- | \( O(d) \). Lookup a value inside 'Treap' by a given index.
at :: forall m a . Int -> Treap m a -> Maybe a
at i t
    | i < 0          = Nothing
    | i >= sizeInt t = Nothing
    | otherwise      = go i t
  where
    go :: Int -> Treap m a -> Maybe a
    go _ Empty = Nothing
    go k (Node _ _ _ a l r) =
        let lSize = sizeInt l
        in case compare k lSize of
            EQ -> Just a
            LT -> go i l
            GT -> go (k - lSize - 1) r

-- | \( O(d) \). Return value of monoidal accumulator on a segment @[l, r)@.
query :: forall m a . Measured m a => Int -> Int -> Treap m a -> m
query from to t
    | to >= from = mempty
    | otherwise  =
        let (_, r) = splitAt from t
            (m, _) = splitAt (to - from) r
        in monoid m

----------------------------------------------------------------------------
-- Cuts and joins
----------------------------------------------------------------------------

-- | Create new 'Node' and recalculate its values.
new :: Measured m a => Priority -> a -> Treap m a -> Treap m a -> Treap m a
new p a l r = recalculate $ Node 0 p mempty a l r
{-# INLINE new #-}

{- | \( O(d) \). @splitAt i t@ splits 'Treap' by the given index into two treaps
@(t1, t2)@ such that the following properties hold:

1. \( \mathrm{size}\ t_1 = i \)
2. \( \mathrm{size}\ t_2 = n - i \)
3. \( \mathrm{merge}\ t_1\ t_2 \equiv t \)

__Special cases:__

1. If \( i \leqslant n \) then the result is @('empty', t)@.
2. If \( i \geqslant n \) then the result is @(t, 'empty')@.
-}
splitAt :: forall m a . Measured m a => Int -> Treap m a -> (Treap m a, Treap m a)
splitAt i t
    | i <= 0         = (empty, t)
    | i >= sizeInt t = (t, empty)
    | otherwise      = go i t
  where
    go :: Int -> Treap m a -> (Treap m a, Treap m a)
    go k = \case
        Empty -> (Empty, Empty)
        Node _ p _ a left right ->
            let lSize = sizeInt left
            in case compare k lSize of
                EQ -> (left, new p a Empty right)
                LT ->
                    let (!t1, !newLeft) = go k left
                    in (t1, new p a newLeft right)
                GT ->
                    let (!newRight, !t2) = go (k - lSize - 1) right
                    in (new p a left newRight, t2)

{- | \( O(\max\ d_1\ d_2) \). Merge two 'Treap's into single one.

>>> pone p a = one (Priority p) a :: Treap (Sum Int) Int
>>> putStrLn $ pretty $ merge (merge (pone 1 3) (pone 4 5)) (merge (pone 3 0) (pone 5 9))
           4,Sum {getSum = 17}:9
                    ╱
          3,Sum {getSum = 8}:5
                   ╱╲
                  ╱  ╲
                 ╱    ╲
                ╱      ╲
               ╱        ╲
              ╱          ╲
             ╱            ╲
            ╱              ╲
           ╱                ╲
          ╱                  ╲
1,Sum {getSum = 3}:3 1,Sum {getSum = 0}:0
-}
merge :: Measured m a => Treap m a -> Treap m a -> Treap m a
merge Empty r = r
merge l Empty = l
merge l@(Node _ p1 _ a1 l1 r1) r@(Node _ p2 _ a2 l2 r2)
    | p1 > p2   = recalculate $ new p1 a1 l1 (merge r1 r)
    | otherwise = recalculate $ new p2 a2 (merge l l2) r2

{- | \( O(d) \). @'take' n t@ returns 'Treap' that contains first @n@ elements of the given
'Treap' @t@.
-}
take :: forall m a . Measured m a => Int -> Treap m a -> Treap m a
take n t
    | n <= 0         = Empty
    | n >= sizeInt t = t
    | otherwise      = go n t
  where
    go :: Int -> Treap m a -> Treap m a
    go _ Empty = Empty
    go 0 _     = Empty
    go i (Node _ p _ a l r) =
        let lSize = sizeInt l
        in case compare i lSize of
            LT -> go i l
            EQ -> l
            GT -> new p a l $ go (i - lSize - 1) r

{- | \( O(d) \). @'drop' n t@ returns 'Treap' without first @n@ elements of the given
'Treap' @t@.
-}
drop :: forall m a . Measured m a => Int -> Treap m a -> Treap m a
drop n t
    | n <= 0         = t
    | n >= sizeInt t = Empty
    | otherwise      = go n t
  where
    go :: Int -> Treap m a -> Treap m a
    go _ Empty = Empty
    go 0 tree  = tree
    go i (Node _ p _ a l r) =
        let lSize = sizeInt l
        in case compare i lSize of
            LT -> new p a (go i l) r
            EQ -> new p a Empty r
            GT -> go (i - lSize - 1) r

{- | \( O(d) \). Rotate a 'Treap' to the right by a given number of elements
modulo treap size. In simple words, @'rotate' n t@ takes first @n@ elements of
@t@ and puts them at the end of @t@ in the same order.
-}
rotate :: forall m a . Measured m a => Int -> Treap m a -> Treap m a
rotate n t = case t of
    Empty -> Empty
    _ | n == 0    -> t
      | otherwise -> let (left, right) = splitAt shift t in merge right left
  where
    shift :: Int
    shift = n `mod` sizeInt t

----------------------------------------------------------------------------
-- Modification functions
----------------------------------------------------------------------------

{- | \( O(d) \). Insert a value into 'Treap' with given key and priority.
Updates monoidal accumulator accordingly.
-}
insert :: forall m a . Measured m a => Int -> Priority -> a -> Treap m a -> Treap m a
insert i p a t
    | i < 0          = go 0 t
    | i >= sizeInt t = go (sizeInt t) t
    | otherwise      = go i t
  where
    go :: Int -> Treap m a -> Treap m a
    go _ Empty = one p a
    go k node@(Node _ tp _ ta l r)
      | p <= tp =
          let lSize = sizeInt l
          in if k <= lSize
              then recalculate $ new tp ta (go k l) r
              else recalculate $ new tp ta l (go (k - lSize - 1) r)
      | otherwise =
          let (!newL, !newR) = splitAt k node
          in recalculate $ new p a newL newR

{- | \( O(d) \). Delete element from 'Treap' by the given index. If index is out
of bounds, 'Treap' remains unchanged.
-}
delete :: forall m a . Measured m a => Int -> Treap m a -> Treap m a
delete i t
    | i < 0          = t
    | i >= sizeInt t = t
    | otherwise      = go i t
  where
    go :: Int -> Treap m a -> Treap m a
    go _ Empty = Empty
    go k (Node _ p _ a l r) =
        let lSize = sizeInt l
        in case compare k lSize of
            EQ -> merge l r
            LT -> recalculate $ new p a (go k l) r
            GT -> recalculate $ new p a l (go (k - lSize - 1) r)

----------------------------------------------------------------------------
-- Core internal functions
----------------------------------------------------------------------------

{- | \( O(1) \). Calculate size and the value of the monoidal accumulator
in the given root node. This function doesn't perform any recursive calls and it
assumes that the values in the children are already correct. So use this
function only in bottom-up manner.
-}
recalculate :: Measured m a => Treap m a -> Treap m a
recalculate Empty = Empty
recalculate (Node _ p _ a l r) =
    Node (1 + size l + size r) p (measure l <> measure a <> measure r) a l r
