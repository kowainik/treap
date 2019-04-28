module Test.Rand.Query
       ( querySpec
       ) where

import Data.Monoid (Sum (..))
import GHC.Exts (IsList (..))
import Test.Hspec (Spec, describe, it, shouldBe)

import Test.Common (TestTreap, extractSum, smallTreap)

import qualified Treap


querySpec :: Spec
querySpec = describe "Query tests" $ do
    basicSpec
    advancedSpec

basicSpec :: Spec
basicSpec = describe "Sanity checks" $ do
    it "size of empty treap is 0" $
        Treap.size Treap.empty `shouldBe` 0
    it "size of singletone treap is 1" $
        Treap.size (Treap.one 1 :: TestTreap) `shouldBe` 1
    it "size of smallTreap is 5" $
        Treap.size smallTreap `shouldBe` 5
    it "measure of empty treap is mempty" $
        extractSum Treap.empty `shouldBe` 0
    it "measure of singleton treap is mempty" $
        extractSum (Treap.one 42) `shouldBe` 42
    it "toList smallTreap is [1..5]" $
        toList smallTreap `shouldBe` [1..5]
    it "total sum of smallTreap is [1..5]" $
        extractSum smallTreap `shouldBe` sum [1..5]

advancedSpec :: Spec
advancedSpec = describe "Different checks on query functions" $ do
    it "elements by indices are correct" $
        map (`Treap.at` smallTreap) [-1..5] `shouldBe`
            [Nothing, Just 1, Just 2, Just 3, Just 4, Just 5, Nothing]
    it "query on empty segment returns 'mempty'" $
        Treap.query 0 (-1) smallTreap `shouldBe` mempty
    it "query on the non-intersected segment returns 'mempty'" $ do
        Treap.query (-2) (-1) smallTreap `shouldBe` mempty
        Treap.query 5 6 smallTreap `shouldBe` mempty
    it "query on subsegments works" $ do
        Treap.query 0 0 smallTreap `shouldBe` 0
        Treap.query 0 1 smallTreap `shouldBe` 1
        Treap.query 0 2 smallTreap `shouldBe` 3
        Treap.query 1 3 smallTreap `shouldBe` 5
        Treap.query (-1) 10 smallTreap `shouldBe` Sum (sum [1..5])
