module Test.Common
       ( TestTreap
       , smallTreap
       , extractSum
       , with
       , describedAs
       ) where

import Data.Monoid (Sum (..))
import GHC.Exts (IsList (..))
import Test.Hspec.Expectations (Expectation, shouldBe)

import Treap (RTreap, measure)


-- | Specialized version of 'RTreap' for testing.
type TestTreap = RTreap (Sum Int) Int

-- | Small treap of total size 5.
smallTreap :: TestTreap
smallTreap = fromList [1..5]

extractSum :: TestTreap -> Int
extractSum = getSum . measure

with :: [Int] -> Int -> ([Int], Sum Int)
with l m = (l, Sum m)
infixr 9 `with`

describedAs :: TestTreap -> ([Int], Sum Int) -> Expectation
describedAs t expectedNodesMeasure
    = treapNodes `with` treapMeasure `shouldBe` expectedNodesMeasure
  where
    treapMeasure :: Int
    treapMeasure = extractSum t
    treapNodes :: [Int]
    treapNodes   = toList t
infixr 8 `describedAs`
