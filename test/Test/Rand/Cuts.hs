module Test.Rand.Cuts
       ( cutsSpec
       ) where

import GHC.Exts (IsList (..))
import Test.Hspec (Spec, describe, it, shouldBe)

import Test.Common (describedAs, with, TestTreap, smallTreap)

import qualified Treap


cutsSpec :: Spec
cutsSpec = describe "Cuts and joins tests" $ do
    splitAtSpec
    mergeSpec
    takeSpec
    dropSpec
    rotateSpec

splitAtSpec :: Spec
splitAtSpec = describe "splitAt" $ do
    it "splitAt negative returns treap itself" $
        snd (Treap.splitAt (-1) smallTreap) `shouldBe` smallTreap
    it "splitAt 0 returns treap itself" $
        snd (Treap.splitAt 0 smallTreap) `shouldBe` smallTreap
    it "splitAt n returns treap itself" $
        fst (Treap.splitAt 5 smallTreap) `shouldBe` smallTreap
    it "splitAt 2 returns two treaps" $ do
        let
            (a, b) = Treap.splitAt 2 smallTreap
        a `describedAs` ([1..2] `with` 3)
        b `describedAs` ([3..5] `with` 12)

mergeSpec :: Spec
mergeSpec = describe "merge" $ do
    it "merge with empty should be treap itself" $ do
        Treap.merge Treap.empty smallTreap `shouldBe` smallTreap
        Treap.merge smallTreap Treap.empty `shouldBe` smallTreap
    it "merge two treaps works" $
        (Treap.merge (fromList [1..2]) (fromList [3..5]) :: TestTreap) `describedAs` ([1..5] `with` 15)

takeSpec :: Spec
takeSpec = describe "take" $ do
    it "take negative returns empty treap" $
       Treap.take (-1) smallTreap `describedAs` ([] `with` 0)
    it "take 0 returns empty treap" $
        Treap.take 0 smallTreap `describedAs` ([] `with` 0)
    it "take size returns treap itself" $
        Treap.take 5 smallTreap `shouldBe` smallTreap
    it "take 2 returns first two elements" $
        Treap.take 2 smallTreap `describedAs` ([1..2] `with` 3)

dropSpec :: Spec
dropSpec = describe "drop" $ do
    it "drop negative returns treap itself" $
       Treap.drop (-1) smallTreap `shouldBe` smallTreap
    it "drop 0 returns treap itself" $
        Treap.drop 0 smallTreap `shouldBe` smallTreap
    it "drop size returns empty treap" $
        Treap.drop 5 smallTreap `describedAs` ([] `with` 0)
    it "drop 2 returns first two elements" $
        Treap.drop 2 smallTreap `describedAs` ([3..5] `with` 12)

rotateSpec :: Spec
rotateSpec = describe "rotate" $ do
    it "rotate 0 does nothing" $
        Treap.rotate 0 smallTreap `shouldBe` smallTreap
    it "rotate 1 moves first element to the end" $
        Treap.rotate 1 smallTreap `describedAs` ([2, 3, 4, 5, 1] `with` 15)
    it "rotate -1 moves last element to the beginning" $
        Treap.rotate (-1) smallTreap `describedAs` ([5, 1, 2, 3, 4] `with` 15)
