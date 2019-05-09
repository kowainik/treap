module Test.Rand.Laws
       ( lawsSpec
       ) where

import GHC.Exts (IsList (..))
import HaskellWorks.Hspec.Hedgehog (require)
import Hedgehog (Gen, forAll, property, (===))
import Test.Hspec (Spec, describe, it)

import Test.Common (TestTreap())

import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range


lawsSpec :: Spec
lawsSpec =
    describe "Law abiding instances" $ do
        semigroupSpec
        monoidSpec

prop_int_list :: Gen [Int]
prop_int_list = Gen.list (Range.linear 1 100) Gen.enumBounded

semigroupSpec :: Spec
semigroupSpec =
    describe "Semigroup" $
        it "associativity" $
            require $ property $ do
                a :: TestTreap <- fromList <$> forAll prop_int_list
                b <- fromList <$> forAll prop_int_list
                c <- fromList <$> forAll prop_int_list
                (a <> b) <> c === a <> (b <> c)

monoidSpec :: Spec
monoidSpec =
    describe "Monoid" $
        it "identity" $
            require $ property $ do
                a :: TestTreap <- fromList <$> forAll prop_int_list
                a <> mempty === a
                mempty <> a === a
