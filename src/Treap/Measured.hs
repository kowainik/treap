{-# LANGUAGE FlexibleInstances #-}

{- | Typeclass that tells how to measure different values as 'Monoid'.
-}

module Treap.Measured
       ( Measured (..)
       ) where

import Data.Functor.Identity (Identity (..))
import Data.Monoid (All (..), Any (..), Dual (..), Endo (..), First (..), Last (..), Product (..),
                    Sum (..))
import Data.Semigroup (Max (..), Min (..))


{- | This typeclass allows to specify how to convert value of type @a@ into
monoidal value of type @m@.
-}
class Monoid m => Measured m a where
    measure :: a -> m

-- | Measure every 'Monoid' as itself.
instance Monoid a => Measured (Identity a) a where
    measure = Identity
    {-# INLINE measure #-}

-- | Measure every 'Monoid' as its dual.
instance Monoid a => Measured (Dual a) a where
    measure = Dual
    {-# INLINE measure #-}

-- | Measure every endomorphic function with compostion.
instance Measured (Endo a) (a -> a) where
    measure = Endo
    {-# INLINE measure #-}

-- | Measure every numeric value with addition.
instance Num a => Measured (Sum a) a where
    measure = Sum
    {-# INLINE measure #-}

-- | Measure every numeric value with multiplication.
instance Num a => Measured (Product a) a where
    measure = Product
    {-# INLINE measure #-}

-- | Measure every comparable value with minimum.
instance (Ord a, Bounded a) => Measured (Min a) a where
    measure = Min
    {-# INLINE measure #-}

-- | Measure every comparable value with maximum.
instance (Ord a, Bounded a) => Measured (Max a) a where
    measure = Max
    {-# INLINE measure #-}

-- | Measure every value as the 'First' monoid.
instance Measured (First a) a where
    measure = First . Just
    {-# INLINE measure #-}

-- | Measure every value as the 'Last' monoid.
instance Measured (Last a) a where
    measure = Last . Just
    {-# INLINE measure #-}

-- | Measure boolean value with '&&'.
instance Measured All Bool where
    measure = All

-- | Measure boolean value with '||'.
instance Measured Any Bool where
    measure = Any
