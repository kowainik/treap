{-# LANGUAGE FlexibleInstances #-}

{- | Typeclass that tells how to convert different values into 'Monoid'.
-}

module Treap.Measured
       ( Measured (..)
       ) where

import Data.Functor.Identity (Identity (..))
import Data.Monoid (Product (..), Sum (..))


class Monoid m => Measured m a where
    measure :: a -> m

-- | Measure every 'Monoid' as itself.
instance Monoid a => Measured (Identity a) a where
    measure = Identity
    {-# INLINE measure #-}

-- -- | Measure everything as @()@.
-- instance Measured () a where
--     measure _ = ()
--     {-# INLINE measure #-}

instance Num a => Measured (Sum a) a where
    measure = Sum
    {-# INLINE measure #-}

instance Num a => Measured (Product a) a where
    measure = Product
    {-# INLINE measure #-}
