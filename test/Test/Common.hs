module Test.Common
       ( TestTreap
       , smallTreap
       , extractSum
       ) where

import Data.Monoid (Sum (..))
import GHC.Exts (IsList (..))

import Treap (RTreap, measure)


-- | Specialized version of 'RTreap' for testing.
type TestTreap = RTreap (Sum Int) Int

-- | Small treap of total size 5.
smallTreap :: TestTreap
smallTreap = fromList [1..5]

extractSum :: TestTreap -> Int
extractSum = getSum . measure
