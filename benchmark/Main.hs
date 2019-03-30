module Main (main) where

import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import Gauge.Main (bench, defaultMain, whnf)
import Treap (HashTreap)

import qualified Data.Map.Strict as Map
import qualified Treap


main :: IO ()
main = do
    let containerMap = Map.fromList $ map (\x -> (x, x)) [1..1000]
    let treapMap = foldl' (\t k -> Treap.insert k k t) Treap.empty [1..1000]
    defaultMain
        [ bench "containers" $ whnf (null . cleanupMap) containerMap
        , bench "treap"      $ whnf (null . cleanupTreap) treapMap
        ]

cleanupMap :: Map Int Int -> Map Int Int
cleanupMap m = foldl' (\t k -> Map.delete k t) m [1..1000]

cleanupTreap :: HashTreap Int Int -> HashTreap Int Int
cleanupTreap m = foldl' (\t k -> Treap.delete k t) m [1..1000]
