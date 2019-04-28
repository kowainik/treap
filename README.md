# treap

[![Hackage](https://img.shields.io/hackage/v/treap.svg)](https://hackage.haskell.org/package/treap)
[![Build status](https://secure.travis-ci.org/chshersh/treap.svg)](https://travis-ci.org/chshersh/treap)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](LICENSE)

Efficient implementation of the implicit treap data structure.

## What this package provides?

This package implements tree-like data structure called _implicit treap_. This
data structure implements interface similar to random-access arrays, but with
fast (logarithmic time complexity)
`insert`/`delete`/`split`/`merge`/`take`/`drop`/`rotate` operations. In addition
_treap_ allows you to specify and measure values of any monoids on a segment,
like sum of elements or minimal element on some contiguous part of the array.

## When to use this package?

Use this package when you want:

1. Access elements by index.
2. Insert elements by index.
3. Delete elements by index.
4. Calculate monoidal operation (like sum, product, min, etc.) of all elements
   between two indices.
5. Call slicing operations like `take` or `drop` or `split`.

Below you can find the table of time complexity for all operations (where `n` is
the size of the treap):

| Operation | Time complexity | Description                          |
|-----------|-----------------|--------------------------------------|
| `size`    | `O(1)`          | Get number of elements in the treap  |
| `at`      | `O(log n)`      | Access by index                      |
| `insert`  | `O(log n)`      | Insert by index                      |
| `delete`  | `O(log n)`      | Delete by index                      |
| `query`   | `O(log n)`      | Measure monoid on the segment        |
| `splitAt` | `O(log n)`      | Split treap by index into two treaps |
| `merge`   | `O(log n)`      | Merge two treaps into single one     |
| `take`    | `O(log n)`      | Take first `i` elements of the treap |
| `drop`    | `O(log n)`      | Drop first `i` elements of the treap |
| `rotate`  | `O(log n)`      | Put first `i` elements to the end    |

## When not to use this package?

If you don't need to calculate monoidal operations, use
[`Seq`](https://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Sequence.html#t:Seq)
from the `containers` package.
