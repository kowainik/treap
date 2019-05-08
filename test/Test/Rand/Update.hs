module Test.Rand.Update
       ( updateSpec
       ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Test.Common (describedAs, with, smallTreap)

import qualified Treap


updateSpec :: Spec
updateSpec = describe "Modification operations tests" $ do
    insertSpec
    deleteSpec

insertSpec :: Spec
insertSpec = describe "insert" $ do
    it "insert negative inserts at the beginning" $
        Treap.insert (-1) 42 smallTreap `describedAs` ((42 : [1..5]) `with` 57)
    it "insert 0 inserts at the beginning" $
        Treap.insert 0 42 smallTreap `describedAs` ((42 : [1..5]) `with` 57)
    it "insert size inserts at the end" $
        Treap.insert 5 42 smallTreap `describedAs` ([1, 2, 3, 4, 5, 42] `with` 57)
    it "insert in the middle works" $
        Treap.insert 2 42 smallTreap `describedAs` ([1, 2, 42, 3, 4, 5] `with` 57)

deleteSpec :: Spec
deleteSpec = describe "delete" $ do
    it "delete negative does nothing" $
        Treap.delete (-1) smallTreap `shouldBe` smallTreap
    it "delete size does nothing" $
        Treap.delete 5 smallTreap `shouldBe` smallTreap
    it "delete 0 removes first element" $
        Treap.delete 0 smallTreap `describedAs` ([2..5] `with` 14)
    it "deletes from the middle works" $
        Treap.delete 2 smallTreap `describedAs` ([1, 2, 4, 5] `with` 12)
