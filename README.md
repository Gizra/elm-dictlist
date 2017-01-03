[![Build Status](https://travis-ci.org/Gizra/elm-dictlist.svg?branch=master)](https://travis-ci.org/Gizra/elm-dictlist)

# elm-dictlist

Have you ever wanted a `Dict`, but you need to maintain an arbitrary
ordering of keys? Or, a `List`, but you want to efficiently lookup values
by a key? With `DictList`, now you can!

`DictList` implements the full API for `Dict` (and should be a drop-in
replacement for it). However, instead of ordering things from lowest
key to highest key, it allows for an arbitrary ordering.

We also implement parts of the API for `List`, often returning a tuple
of (key, value) pairs.

An alternative would be to maintain your own "association list" -- that is,
a `List (k, v)` instead of a `DictList k v`. It should be the case that
`DictList.get` and `DictList.member` perform better than the equivalent
operations on an association list, but other functions may be slower.

The code is not as optimized as it could be, yet -- we'll work on covering
the `List` API more fully first, and then work on various possible optimizations.

For the detailed API, see the
[Elm package site](http://package.elm-lang.org/packages/gizra/elm-dict-list/latest),
or the links to the right, if you're already there.
