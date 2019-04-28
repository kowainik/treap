module Test.Rand.Update
       ( updateSpec
       ) where

import GHC.Exts (IsList (..))
import Test.Hspec (Spec, describe, it, shouldBe)

import Test.Common (smallTreap)

import qualified Treap


updateSpec :: Spec
updateSpec = describe "Modification operations tests" $ do
    insertSpec
    deleteSpec

insertSpec :: Spec
insertSpec = describe "insert" $ do
    it "insert negative inserts at the beginning" $
        toList (Treap.insert (-1) 42 smallTreap) `shouldBe` 42 : [1..5]
    it "insert 0 inserts at the beginning" $
        toList (Treap.insert 0 42 smallTreap) `shouldBe` 42 : [1..5]
    it "insert size inserts at the end" $
        toList (Treap.insert 5 42 smallTreap) `shouldBe` [1, 2, 3, 4, 5, 42]
    it "insert in the middle works" $
        toList (Treap.insert 2 42 smallTreap) `shouldBe` [1, 2, 42, 3, 4, 5]

deleteSpec :: Spec
deleteSpec = describe "delete" $ do
    it "delete negative does nothing" $
        Treap.delete (-1) smallTreap `shouldBe` smallTreap
    it "delete size does nothing" $
        Treap.delete 5 smallTreap `shouldBe` smallTreap
    it "delete 0 removes first element" $
        toList (Treap.delete 0 smallTreap) `shouldBe` [2..5]
    it "deletes from the middle works" $
        toList (Treap.delete 2 smallTreap) `shouldBe` [1, 2, 4, 5]
