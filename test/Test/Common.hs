module Test.Common
       ( TestTreap
       , smallTreap
       , extractSum
       , emptyTreap
       ) where

import Data.Monoid (Sum (..))
import GHC.Exts (IsList (..))

import Treap (empty, RTreap, measure)


-- | Specialized version of 'RTreap' for testing.
type TestTreap = RTreap (Sum Int) Int

-- | Small treap of total size 5.
smallTreap :: TestTreap
smallTreap = fromList [1..5]

emptyTreap :: TestTreap
emptyTreap = empty

extractSum :: TestTreap -> Int
extractSum = getSum . measure
