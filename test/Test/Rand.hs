module Test.Rand
       ( randSpec
       ) where

import Test.Hspec (Spec, describe)

import Test.Rand.Cuts (cutsSpec)
import Test.Rand.Laws (lawsSpec)
import Test.Rand.Query (querySpec)
import Test.Rand.Update (updateSpec)


randSpec :: Spec
randSpec = describe "RTreap" $ do
    querySpec
    cutsSpec
    updateSpec
    lawsSpec
