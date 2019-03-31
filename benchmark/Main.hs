module Main (main) where

import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import Gauge.Main (bench, defaultMain, whnf)
import Treap.Rand (RandTreap)

import qualified Data.Map.Strict as Map
import qualified Treap.Rand as Rand


main :: IO ()
main = do
    let containerMap = Map.fromList $ map (\x -> (x, x)) [1..1000]
    let randTreapMap = foldl' (\t k -> Rand.insert k k t) Rand.empty [1..1000]
    defaultMain
        [ bench "containers" $ whnf (null . cleanupMap) containerMap
        , bench "rand-treap" $ whnf (null . cleanupRand) randTreapMap
        ]

cleanupMap :: Map Int Int -> Map Int Int
cleanupMap m = foldl' (\t k -> Map.delete k t) m [1..1000]

cleanupRand :: RandTreap Int Int -> RandTreap Int Int
cleanupRand m = foldl' (\t k -> Rand.delete k t) m [1..1000]
