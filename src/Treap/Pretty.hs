-- | Very nice 'Treap' visualisation.

module Treap.Pretty
       ( pretty
       , prettyPrint
       , prettyWith
       , compactShowNode

         -- * Internal implementation details
       , BinTree (..)
       , showTree
       , middleLabelPos
       , branchLines
       ) where

import Data.Char (isSpace)
import Data.Coerce (Coercible, coerce)
import Data.List (dropWhileEnd, intercalate)

import Treap.Pure (Priority (..), Size (..), Treap (..))


{- | Show 'Treap' in a pretty way using 'compactShowNode' function.
-}
pretty :: forall m a . (Coercible m a, Show a) => Treap m a -> String
pretty = prettyWith compactShowNode

prettyPrint :: forall m a . (Coercible m a, Show a) => Treap m a -> IO ()
prettyPrint = putStrLn . pretty

{- | Show 'Treap' node in a format:

@
<size>,<acc>:a
@
-}
compactShowNode
    :: forall m a . (Coercible m a, Show a)
    => Size
    -> Priority
    -> m
    -> a
    -> String
compactShowNode (Size sz) _ m a =
    show sz ++ "," ++ show (coerce @m @a m) ++ ":" ++ show a

-- | Show 'Treap' in a nice way using given function to display node.
prettyWith
    :: forall m a .
       (Size -> Priority -> m -> a -> String)
    -> Treap m a
    -> String
prettyWith display = showTree . toBinTree
  where
    toBinTree :: Treap m a -> BinTree
    toBinTree Empty = Leaf
    toBinTree (Node sz p m a left right) = Branch (display sz p m a) (toBinTree left) (toBinTree right)

-- | Intermidiate structure to help string conversion.
data BinTree
    = Leaf
    | Branch String BinTree BinTree

showTree :: BinTree -> String
showTree Leaf                  = ""
showTree (Branch label left right) = case (left, right) of
    (Leaf, Leaf) -> label

    (_, Leaf) -> toLines $
        [ spaces rootShiftOnlyLeft   ++ label
        , spaces branchShiftOnlyLeft ++ "╱"
        ] ++ map (spaces leftShiftOnlyLeft ++) leftLines

    (Leaf, _) -> toLines $
        [ spaces rootShiftOnlyRight   ++ label
        , spaces branchShiftOnlyRight ++ "╲"
        ] ++ map (spaces rightShiftOnlyRight ++) rightLines

    (_, _) -> toLines $
        [ spaces rootOffset ++ label
        ]
        ++ map (spaces rootOffset ++ ) (branchLines branchHeight)
        ++ map (spaces childrenOffset ++) (zipChildren leftLines rightLines)
  where
    leftStr, rightStr :: String
    leftStr  = showTree left
    rightStr = showTree right

    leftLines :: [String]
    leftLines  = lines leftStr
    rightLines = lines rightStr

    rootLabelMiddle, leftLabelMiddle, rightLabelMiddle :: Int
    rootLabelMiddle  = middleLabelPos label
    leftLabelMiddle  = middleLabelPos $ head leftLines
    rightLabelMiddle = middleLabelPos $ head rightLines

    -- Case 1: all offsets when node has only left branch
    rootShiftOnlyLeft, leftShiftOnlyLeft, branchShiftOnlyLeft :: Int
    (rootShiftOnlyLeft, leftShiftOnlyLeft) = case compare rootLabelMiddle leftLabelMiddle of
        EQ -> (1, 0)
        GT -> (0, rootLabelMiddle - leftLabelMiddle - 1)
        LT -> (leftLabelMiddle - rootLabelMiddle + 1, 0)
    branchShiftOnlyLeft = rootLabelMiddle + rootShiftOnlyLeft - 1

    -- Case 2: all offsets when node has only right branch
    rootShiftOnlyRight, rightShiftOnlyRight, branchShiftOnlyRight :: Int
    (rootShiftOnlyRight, rightShiftOnlyRight) = case compare rootLabelMiddle rightLabelMiddle of
        EQ -> (0, 1)
        GT -> (0, rootLabelMiddle - rightLabelMiddle + 1)
        LT -> (rightLabelMiddle - rootLabelMiddle - 1, 0)
    branchShiftOnlyRight = rootLabelMiddle + rootShiftOnlyRight + 1

    -- Case 3: both
    leftWidth, rightOffMiddle, childDistance, branchHeight, rootMustMiddle :: Int
    leftWidth      = 1 + maximum (map length leftLines)
    rightOffMiddle = leftWidth + rightLabelMiddle
    childDistance  = rightOffMiddle - leftLabelMiddle
    branchHeight   = childDistance `div` 2
    rootMustMiddle = (leftLabelMiddle + rightOffMiddle) `div` 2

    rootOffset, childrenOffset :: Int
    (rootOffset, childrenOffset) = case compare rootLabelMiddle rootMustMiddle of
        EQ -> (0, 0)
        LT -> (rootMustMiddle - rootLabelMiddle, 0)
        GT -> (0, rootLabelMiddle - rootMustMiddle)

    zipChildren :: [String] -> [String] -> [String]
    zipChildren l []          = l
    zipChildren [] r          = map (spaces leftWidth ++ ) r
    zipChildren (x:xs) (y:ys) =
        let xLen = length x
            newX = x ++ spaces (leftWidth - xLen)
        in (newX ++ y) : zipChildren xs ys

-- | Generates strings containing of @n@ spaces.
spaces :: Int -> String
spaces n = replicate n ' '

{- | Calculates position of middle of non-space part of the string.

>>> s = "   abc "
>>> length s
7
>>> middleLabelPos s
4
-}
middleLabelPos :: String -> Int
middleLabelPos s =
    let (spacePrefix, rest) = span isSpace s
    in length spacePrefix + (length (dropWhileEnd isSpace rest) `div` 2)

-- | Like 'unlines' but doesn't add "\n" to the end.
toLines :: [String] -> String
toLines = intercalate "\n"

{- | Draws branches of the given height.

>>> putStrLn $ toLines $ branchLines 1
╱╲

>>> putStrLn $ toLines $ branchLines 2
 ╱╲
╱  ╲

>>> putStrLn $ toLines $ branchLines 3
  ╱╲
 ╱  ╲
╱    ╲
-}
branchLines :: Int -> [String]
branchLines n = go 0
  where
    go :: Int -> [String]
    go i
        | i == n    = []
        | otherwise = line : go (i + 1)
      where
        line :: String
        line = spaces (n - i - 1) ++ "╱" ++ spaces (2 * i) ++ "╲"
