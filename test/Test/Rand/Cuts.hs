module Test.Rand.Cuts
       ( cutsSpec
       ) where

import Data.Bifunctor (bimap)
import GHC.Exts (IsList (..))
import Test.Hspec (Spec, describe, it, shouldBe)

import Test.Common (TestTreap, smallTreap)

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
    it "splitAt 2 returns two treaps" $
        bimap toList toList (Treap.splitAt 2 smallTreap) `shouldBe` ([1..2], [3..5])

mergeSpec :: Spec
mergeSpec = describe "merge" $ do
    it "merge with empty should be treap itself" $ do
        Treap.merge Treap.empty smallTreap `shouldBe` smallTreap
        Treap.merge smallTreap Treap.empty `shouldBe` smallTreap
    it "merge two treaps works" $
        toList (Treap.merge (fromList [1..2]) (fromList [3..5]) :: TestTreap) `shouldBe` [1..5]

takeSpec :: Spec
takeSpec = describe "take" $ do
    it "take negative returns empty treap" $
       Treap.take (-1) smallTreap `shouldBe` Treap.empty
    it "take 0 returns empty treap" $
        Treap.take 0 smallTreap `shouldBe` Treap.empty
    it "take size returns treap itself" $
        Treap.take 5 smallTreap `shouldBe` smallTreap
    it "take 2 returns first two elements" $
        Treap.take 2 smallTreap `shouldBe` fromList [1..2]

dropSpec :: Spec
dropSpec = describe "drop" $ do
    it "drop negative returns treap itself" $
       Treap.drop (-1) smallTreap `shouldBe` smallTreap
    it "drop 0 returns treap itself" $
        Treap.drop 0 smallTreap `shouldBe` smallTreap
    it "drop size returns empty treap" $
        Treap.drop 5 smallTreap `shouldBe` Treap.empty
    it "drop 2 returns first two elements" $
        toList (Treap.drop 2 smallTreap) `shouldBe` [3..5]

rotateSpec :: Spec
rotateSpec = describe "rotate" $ do
    it "rotate 0 does nothing" $
        Treap.rotate 0 smallTreap `shouldBe` smallTreap
    it "rotate 1 moves first element to the end" $
        toList (Treap.rotate 1 smallTreap) `shouldBe` [2, 3, 4, 5, 1]
    it "rotate -1 moves last element to the beginning" $
        toList (Treap.rotate (-1) smallTreap) `shouldBe` [5, 1, 2, 3, 4]
