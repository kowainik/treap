{- |

== General description

Package @treap@ implements tree-like data structure called /implicit treap/. This
data structure implements interface similar to random-access arrays, but with
fast (logarithmic time complexity)
@insert@ \/ @delete@ \/ @split@ \/ @merge@ \/ @take@ \/ @drop@ \/ @rotate@ operations.

In addition 'RTreap' allows you to specify and measure values of any monoids on a segment,
like sum of elements or minimal element on some contiguous part of the array.

== Package structure

This package contains the following modules:

* __"Treap.Measured":__ typeclass 'Measured' that allows to tell how to measure
  tree values as monoids.
* __"Treap.Pure":__ the 'Treap.Pure.Treap' data type and functions – pure
  implementation of the implicit treap data structure.
* __"Treap.Rand":__ the 'RTreap' data type and functions – pure implementation
  of the implicit treap which uses pure random generator to automatically
  generate prioriorities.
* __"Treap.Pretty":__ pretty-printer for the treap.

Module __"Treap"__ reexports only __"Treap.Measured"__ and __"Treap.Rand"__ modules.

== Usage example

Consider the following example of creating 'RTreap' from list @[1..5]@ where
each element stores the sum of elements in its subtree:

>>> import Data.Monoid (Sum (..))
>>> import GHC.Exts (IsList (..))
>>> t = fromList [1..5] :: RTreap (Sum Int) Int
>>> prettyPrint t
   5,15:2
      ╱╲
     ╱  ╲
    ╱    ╲
   ╱      ╲
1,1:1   3,12:4
          ╱╲
         ╱  ╲
        ╱    ╲
      1,3:3 1,5:5

Each node shows:

1. Overall size of the tree
2. Total monoidal measure of the tree
3. The element itself after @:@

You can try to play with this tree now!

>>> at 0 t
Just 1
>>> at 10 t
Nothing
>>> query 1 4 t
Sum {getSum = 9}

>>> prettyPrint $ Treap.take 2 t
 2,3:2
  ╱
1,1:1

>>> prettyPrint $ Treap.drop 2 t
  3,12:4
    ╱╲
   ╱  ╲
  ╱    ╲
1,3:3 1,5:5

>>> prettyPrint $ rotate 2 t
   5,15:2
     ╱
  4,13:4
    ╱╲
   ╱  ╲
  ╱    ╲
1,3:3 2,6:5
         ╲
       1,1:1

-}

module Treap
       ( module Treap
       ) where

import Treap.Measured as Treap
import Treap.Rand as Treap
