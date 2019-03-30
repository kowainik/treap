{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies      #-}

{- | Implementation of the 'Treap' data structure where priority is a hash of a
key. True 'Treap' uses random priority upon data structure insertion. However,
in order to keep implementation pure, this module calculates priority from the
key using 'Hashable' typeclass.
-}

module Treap.Hash
       ( -- * Data structure
         HashTreap (..)

         -- * Smart constructors
       , emptyWithSalt
       , oneWithSalt
       , empty
       , one
       , defaultSalt

         -- * Interface functions
       , insert
       , delete
       ) where

import Data.Foldable (foldl')
import Data.Hashable (Hashable (hashWithSalt))
import GHC.Generics (Generic)

import Treap.Pure (Treap)

import qualified Treap.Pure as Treap

----------------------------------------------------------------------------
-- Data structure and instances
----------------------------------------------------------------------------

{- | Specialized version of 'Treap' where priority has type 'Int' and is a hash
of a corresponding key.
-}
data HashTreap k a = HashTreap
    { hashTreapSalt :: !Int
    , hashTreapTree :: !(Treap k Int a)
    } deriving (Show, Read, Eq, Generic, Functor, Foldable, Traversable)

----------------------------------------------------------------------------
-- Smart constructors
----------------------------------------------------------------------------

-- | Default value of salt. Used for pure computations.
defaultSalt :: Int
defaultSalt = 16777619

-- | \( O(1) \). Create empty 'HashTreap' with given salt.
emptyWithSalt :: Int -> HashTreap k a
emptyWithSalt salt = HashTreap salt Treap.Empty
{-# INLINE emptyWithSalt #-}

-- | \( O(1) \). Create empty 'HashTreap' using 'defaultSalt'.
empty :: HashTreap k a
empty = emptyWithSalt defaultSalt
{-# INLINE empty #-}

-- | \( O(1) \). Create singleton 'HashTreap' with given salt.
oneWithSalt :: Hashable k => Int -> k -> a -> HashTreap k a
oneWithSalt salt k a = HashTreap salt $ Treap.one k (hashWithSalt salt k) a
{-# INLINE oneWithSalt #-}

-- | \( O(1) \). Create singleton 'HashTreap' using 'defaultSalt'.
one :: Hashable k => k -> a -> HashTreap k a
one = oneWithSalt defaultSalt
{-# INLINE one #-}

----------------------------------------------------------------------------
-- Core functions
----------------------------------------------------------------------------

-- | \( O(d) \). Insert a value into 'HashTreap' by given key.
insert :: (Ord k, Hashable k) => k -> a -> HashTreap k a -> HashTreap k a
insert k a (HashTreap salt t) =
    HashTreap salt $ Treap.insert k (hashWithSalt salt k) a t
{-# INLINE insert #-}

{- | \( O(d) \). Delete 'HashTreap' node that contains given key. If there is no
such key, 'HashTreap' remains unchanged.
-}
delete :: Ord k => k -> HashTreap k a -> HashTreap k a
delete k (HashTreap salt t) = HashTreap salt $ Treap.delete k t
{-# INLINE delete #-}
