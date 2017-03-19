module DictList
    exposing
        ( DictList
          -- originally from `Dict`
        , empty
        , singleton
        , insert
        , update
        , isEmpty
        , get
        , remove
        , member
        , size
        , filter
        , partition
        , foldl
        , foldr
        , map
        , union
        , intersect
        , diff
        , merge
        , keys
        , values
        , toList
        , fromList
          -- core `List`
        , cons
        , head
        , tail
        , indexedMap
        , filterMap
        , length
        , reverse
        , all
        , any
        , append
        , concat
        , sum
        , product
        , maximum
        , minimum
        , take
        , drop
        , sort
        , sortBy
        , sortWith
          -- list-oriented
        , getAt
        , getKeyAt
        , indexOfKey
        , insertAfter
        , insertBefore
        , next
        , previous
          -- JSON
        , decodeObject
        , decodeWithKeys
        , decodeKeysAndValues
        , decodeArray
          -- Conversion
        , toDict
        , fromDict
          -- list-extra
        , last
        , inits
        , (!!)
        , uncons
        , maximumBy
        , minimumBy
        , andMap
        , andThen
        , takeWhile
        , dropWhile
        , unique
        , uniqueBy
        , allDifferent
        , allDifferentBy
        )

{-| Have you ever wanted a `Dict`, but you need to maintain an arbitrary
ordering of keys? Or, a `List`, but you want to efficiently lookup values
by a key? With `DictList`, now you can!

`DictList` implements the full API for `Dict` (and should be a drop-in
replacement for it). However, instead of ordering things from lowest
key to highest key, it allows for an arbitrary ordering.

We also implement most of the API for `List`. However, the API is not
identical, since we need to account for both keys and values.

An alternative would be to maintain your own "association list" -- that is,
a `List (k, v)` instead of a `DictList k v`. You can move back and forth
between an association list and a `DictList` via `toList` and `fromList`.

# DictList

@docs DictList

# Build

@docs empty, singleton, insert, update, remove
@docs cons, insertAfter, insertBefore

# Combine

@docs append, concat
@docs union, intersect, diff, merge

# Query

@docs isEmpty, member, get, size
@docs getAt, getKeyAt, indexOfKey
@docs take, drop, next, previous
@docs length, head, tail
@docs all, any
@docs sum, product, maximum, minimum

# Transform

@docs map, foldl, foldr, filter, partition
@docs indexedMap, filterMap, reverse
@docs sort, sortBy, sortWith

# Convert

@docs keys, values, toList, fromList
@docs toDict, fromDict

# JSON

@docs decodeObject, decodeArray, decodeWithKeys, decodeKeysAndValues

# ListExtra

@docs last, inits, (!!), uncons, maximumBy, minimumBy, andMap, andThen, takeWhile, dropWhile, unique, uniqueBy, allDifferent, allDifferentBy

-}


import Dict exposing (Dict, keys)
import DictList.Compat exposing (customDecoder, decodeAndThen, first, maybeAndThen, second)
import Json.Decode exposing (Decoder, keyValuePairs, value, decodeValue)
import List.Extra
import Set


{-| A `Dict` that maintains an arbitrary ordering of keys (rather than sorting
them, as a normal `Dict` does. Or, a `List` that permits efficient lookup of
values by a key. You can look at it either way.
-}
type DictList k v
    = DictList (Dict k v) (List k)



{- I considered something like this instead:

       type DictList k v = DictList (Dict k v) (List (k, v))

   This would speed up some things, because our `List` would have the values
   as well -- we wouldn't have to look them up in the `Dict` when doing
   list-oriented things. However, it would slow down other things, because
   we'd have to modify the list in cases where only the value was changing,
   not the key. So, it's something we could reconsider depending on
   desired performance characteristics.

   Another performance issue down the road would be whether to use `Array`
   for the internal implementation rather than `List`.
-}
--
-------
-- JSON
-------


{-| Turn any object into a dictionary of key-value pairs, including inherited
enumerable properties. Fails if _any_ value can't be decoded with the given
decoder.

Unfortunately, it is not possible to preserve the apparent order of the keys in
the JSON, because the keys in Javascript objects are fundamentally un-ordered.
Thus, you will typically need to use `decodeWithKeys` or `decodeArray` instead.
-}
decodeObject : Decoder a -> Decoder (DictList String a)
decodeObject decoder =
    Json.Decode.map fromList (keyValuePairs decoder)


{-| This function produces a decoder you can use if you can decode a list of your keys,
and given a key, you can produce a decoder for the corresponding value. The
order within the `DictList` will be the order of your list of keys.
-}
decodeWithKeys : List comparable -> (comparable -> Decoder value) -> Decoder (DictList comparable value)
decodeWithKeys keys func =
    let
        go jsonValue key accum =
            case ( accum, decodeValue (func key) jsonValue ) of
                ( Ok goodSoFar, Ok thisTime ) ->
                    -- If we've been successful so far, and OK this time, then accumulate
                    Ok <| insert key thisTime goodSoFar

                ( Ok goodSoFar, Err err ) ->
                    -- If we were OK until now, but this one erred, then the whole thing fails
                    Err err

                ( Err err, Ok _ ) ->
                    -- If we've already had an error, but this one is good, just keep the error
                    accum

                ( Err err1, Err err2 ) ->
                    -- If we had an error, and we have another one, combine them
                    Err <| err1 ++ "\n" ++ err2
    in
        customDecoder value
            (\jsonValue -> List.foldl (go jsonValue) (Ok empty) keys)


{-| Like `decodeWithKeys`, but you supply a decoder for the keys, rather than the keys themselves.

Note that the starting point for all decoders will be the same place, so you need to construct your
decoders in a way that makes that work.
-}
decodeKeysAndValues : Decoder (List comparable) -> (comparable -> Decoder value) -> Decoder (DictList comparable value)
decodeKeysAndValues keyDecoder func =
    keyDecoder
        |> decodeAndThen (\keys -> decodeWithKeys keys func)


{-| Given a decoder for the value, and a way of turning the value into a key,
decode an array of values into a `DictList`. The order within the `DictList`
will be the order of the JSON array.
-}
decodeArray : (value -> comparable) -> Decoder value -> Decoder (DictList comparable value)
decodeArray keyMapper valueDecoder =
    Json.Decode.map
        (List.map (\value -> ( keyMapper value, value )) >> fromList)
        (Json.Decode.list valueDecoder)



----------------------
-- From `List` in core
----------------------


{-| Insert a key-value pair at the front. Moves the key to the front if
    it already exists.
-}
cons : comparable -> value -> DictList comparable value -> DictList comparable value
cons key value (DictList dict list) =
    let
        restOfList =
            if Dict.member key dict then
                List.Extra.remove key list
            else
                list
    in
        DictList
            (Dict.insert key value dict)
            (key :: restOfList)


{-| Gets the first key with its value.
-}
head : DictList comparable value -> Maybe ( comparable, value )
head (DictList dict list) =
    List.head list
        |> maybeAndThen (\key -> Dict.get key dict |> Maybe.map (\value -> ( key, value )))


{-| Extract the rest of the `DictList`, without the first key/value pair.
-}
tail : DictList comparable value -> Maybe (DictList comparable value)
tail (DictList dict list) =
    case list of
        first :: rest ->
            Just <|
                DictList (Dict.remove first dict) rest

        _ ->
            Nothing


{-| Like `map` but the function is also given the index of each
element (starting at zero).
-}
indexedMap : (Int -> comparable -> a -> b) -> DictList comparable a -> DictList comparable b
indexedMap func =
    let
        go key value ( index, DictList dict list ) =
            ( index + 1
            , DictList
                (Dict.insert key (func index key value) dict)
                (key :: list)
            )
    in
        -- We need to foldl, because the first element should get the 0 index.
        -- But we build up the resulting list with `::`, for efficiency, so
        -- we reverse once at the end.
        foldl go ( 0, empty ) >> second >> reverse


{-| Apply a function that may succeed to all key-value pairs, but only keep
the successes.
-}
filterMap : (comparable -> a -> Maybe b) -> DictList comparable a -> DictList comparable b
filterMap func =
    let
        go key value acc =
            func key value
                |> Maybe.map (\result -> cons key result acc)
                |> Maybe.withDefault acc
    in
        foldr go empty


{-| The number of key-value pairs in the `DictList`.
-}
length : DictList key value -> Int
length =
    size


{-| Reverse the order of the key-value pairs.
-}
reverse : DictList key value -> DictList key value
reverse (DictList dict list) =
    DictList dict (List.reverse list)


{-| Determine if all elements satisfy the predicate.
-}
all : (comparable -> value -> Bool) -> DictList comparable value -> Bool
all func dictList =
    not (any (\key value -> not (func key value)) dictList)


{-| Determine if any elements satisfy the predicate.
-}
any : (comparable -> value -> Bool) -> DictList comparable value -> Bool
any func (DictList dict list) =
    let
        go innerList =
            case innerList of
                [] ->
                    False

                first :: rest ->
                    if func first (unsafeGet first dict) then
                        True
                    else
                        go rest
    in
        go list


{-| Put two dictionaries together.

If keys collide, preference is given to the value from the second `DictList`.
Also, the order of the keys in the second `DictList` will be preserved at the
end of the result.

So, you could think of `append` as biased towards the second argument. The end
of the result should be equal to the second argument, both in value and key-order.
The front of the result will then be whatever is left from the first argument --
that is, those keys (and their values) that were not in the second argument.

For a similar function that is biased towards the first argument, see `union`.
-}
append : DictList comparable value -> DictList comparable value -> DictList comparable value
append t1 t2 =
    let
        go key value acc =
            -- We're right-favouring, so only act if the key is not already present
            if member key acc then
                acc
            else
                cons key value acc
    in
        foldr go t2 t1


{-| Concatenate a bunch of dictionaries into a single dictionary.

Works from left to right, applying `append` as it goes.
-}
concat : List (DictList comparable value) -> DictList comparable value
concat lists =
    List.foldr append empty lists


{-| Get the sum of the values.
-}
sum : DictList comparable number -> number
sum (DictList dict list) =
    Dict.foldl (always (+)) 0 dict


{-| Get the product of the values.
-}
product : DictList comparable number -> number
product (DictList dict list) =
    Dict.foldl (always (*)) 1 dict


{-| Find the maximum value. Returns `Nothing` if empty.
-}
maximum : DictList comparable1 comparable2 -> Maybe comparable2
maximum (DictList dict list) =
    -- I considered having `maximum` and `minimum` return the key
    -- as well, but there is a bit of a puzzle there. What would
    -- one do when there are ties for the maximum value?
    let
        go _ value acc =
            case acc of
                Nothing ->
                    Just value

                Just bestSoFar ->
                    Just <| max bestSoFar value
    in
        Dict.foldl go Nothing dict


{-| Find the minimum value. Returns `Nothing` if empty.
-}
minimum : DictList comparable1 comparable2 -> Maybe comparable2
minimum (DictList dict list) =
    let
        go _ value acc =
            case acc of
                Nothing ->
                    Just value

                Just bestSoFar ->
                    Just <| min bestSoFar value
    in
        Dict.foldl go Nothing dict


{-| Take the first *n* values.
-}
take : Int -> DictList comparable value -> DictList comparable value
take n (DictList dict list) =
    let
        newList =
            List.take n list

        newDict =
            List.foldl go Dict.empty newList

        go key =
            Dict.insert key (unsafeGet key dict)
    in
        DictList newDict newList


{-| Drop the first *n* values.
-}
drop : Int -> DictList comparable value -> DictList comparable value
drop n (DictList dict list) =
    let
        newList =
            List.drop n list

        newDict =
            List.foldl go Dict.empty newList

        go key =
            Dict.insert key (unsafeGet key dict)
    in
        DictList newDict newList


{-| Sort values from lowest to highest
-}
sort : DictList comparable1 comparable2 -> DictList comparable1 comparable2
sort dictList =
    case dictList of
        DictList dict list ->
            toList dictList
                |> List.sortBy second
                |> List.map first
                |> DictList dict


{-| Sort values by a derived property.
-}
sortBy : (value -> comparable) -> DictList comparable2 value -> DictList comparable2 value
sortBy func dictList =
    case dictList of
        DictList dict list ->
            toList dictList
                |> List.sortBy (func << second)
                |> List.map first
                |> DictList dict


{-| Sort values with a custom comparison function.
-}
sortWith : (value -> value -> Order) -> DictList comparable value -> DictList comparable value
sortWith func dictList =
    case dictList of
        DictList dict list ->
            toList dictList
                |> List.sortWith (\v1 v2 -> func (second v1) (second v2))
                |> List.map first
                |> DictList dict



----------------
-- List-oriented
----------------


{-| Given a key, what index does that key occupy (0-based) in the
order maintained by the `DictList`?
-}
indexOfKey : comparable -> DictList comparable value -> Maybe Int
indexOfKey key (DictList dict list) =
    List.Extra.elemIndex key list


{-| Given a key, get the key and value at the next position.
-}
next : comparable -> DictList comparable value -> Maybe ( comparable, value )
next key dictlist =
    indexOfKey key dictlist
        |> maybeAndThen (\index -> getAt (index + 1) dictlist)


{-| Given a key, get the key and value at the previous position.
-}
previous : comparable -> DictList comparable value -> Maybe ( comparable, value )
previous key dictlist =
    indexOfKey key dictlist
        |> maybeAndThen (\index -> getAt (index - 1) dictlist)


{-| Gets the key at the specified index (0-based).
-}
getKeyAt : Int -> DictList key value -> Maybe key
getKeyAt index (DictList dict list) =
    List.Extra.getAt index list


{-| Gets the key and value at the specified index (0-based).
-}
getAt : Int -> DictList comparable value -> Maybe ( comparable, value )
getAt index (DictList dict list) =
    List.Extra.getAt index list
        |> maybeAndThen
            (\key ->
                Dict.get key dict
                    |> Maybe.map (\value -> ( key, value ))
            )

{-| Alias for getAt, but with the parameters flipped.
-}
(!!) : DictList comparable value -> Int -> Maybe ( comparable, value)
(!!) =
    flip getAt

{-| Insert a key-value pair into a `DictList`, replacing an existing value if
the keys collide. The first parameter represents an existing key, while the
second parameter is the new key. The new key and value will be inserted after
the existing key (even if the new key already exists). If the existing key
cannot be found, the new key/value pair will be inserted at the end.
-}
insertAfter : comparable -> comparable -> v -> DictList comparable v -> DictList comparable v
insertAfter afterKey key value (DictList dict list) =
    let
        newDict =
            Dict.insert key value dict

        newList =
            if afterKey == key then
                -- If we want to insert it after itself, we can short-circuit
                list
            else
                let
                    listWithoutKey =
                        if Dict.member key dict then
                            List.Extra.remove key list
                        else
                            -- If the key wasn't present, we can skip the removal
                            list
                in
                    case List.Extra.elemIndex afterKey listWithoutKey of
                        Just index ->
                            -- We found the existing element, so take apart the list
                            -- and put it back together
                            List.take (index + 1) listWithoutKey
                                ++ (key :: List.drop (index + 1) listWithoutKey)

                        Nothing ->
                            -- The afterKey wasn't found, so we insert the key at the end
                            listWithoutKey ++ [ key ]
    in
        DictList newDict newList


{-| Insert a key-value pair into a `DictList`, replacing an existing value if
the keys collide. The first parameter represents an existing key, while the
second parameter is the new key. The new key and value will be inserted before
the existing key (even if the new key already exists). If the existing key
cannot be found, the new key/value pair will be inserted at the beginning.
-}
insertBefore : comparable -> comparable -> v -> DictList comparable v -> DictList comparable v
insertBefore beforeKey key value (DictList dict list) =
    let
        newDict =
            Dict.insert key value dict

        newList =
            if beforeKey == key then
                -- If we want to insert it before itself, we can short-circuit
                list
            else
                let
                    listWithoutKey =
                        if Dict.member key dict then
                            List.Extra.remove key list
                        else
                            -- If the key wasn't present, we can skip the removal
                            list
                in
                    case List.Extra.elemIndex beforeKey listWithoutKey of
                        Just index ->
                            -- We found the existing element, so take apart the list
                            -- and put it back together
                            List.take index listWithoutKey
                                ++ (key :: List.drop index listWithoutKey)

                        Nothing ->
                            -- The beforeKey wasn't found, so we insert the key at the beginning
                            key :: listWithoutKey
    in
        DictList newDict newList



--------------
-- From `Dict`
--------------


{-| Create an empty `DictList`.
-}
empty : DictList k v
empty =
    DictList Dict.empty []


{-| Get the value associated with a key. If the key is not found, return
`Nothing`.
-}
get : comparable -> DictList comparable v -> Maybe v
get key (DictList dict list) =
    -- So, this is basically the key thing that is optimized, compared
    -- to an association list.
    Dict.get key dict


{-| Determine whether a key is in the `DictList`.
-}
member : comparable -> DictList comparable v -> Bool
member key (DictList dict list) =
    Dict.member key dict


{-| Determine the number of key-value pairs in the `DictList`.
-}
size : DictList k v -> Int
size (DictList dict list) =
    Dict.size dict


{-| Determine whether a `DictList` is empty.
-}
isEmpty : DictList k v -> Bool
isEmpty (DictList dict list) =
    List.isEmpty list


{-| Insert a key-value pair into a `DictList`. Replaces the value when the
keys collide, leaving the keys in the same order as they had been in.
If the key did not previously exist, it is added to the end of
the list.
-}
insert : comparable -> v -> DictList comparable v -> DictList comparable v
insert key value (DictList dict list) =
    let
        newDict =
            Dict.insert key value dict

        newList =
            if Dict.member key dict then
                -- We know this key, so leave it where it was
                list
            else
                -- We don't know this key, so also insert it at the end of the list.
                list ++ [ key ]
    in
        DictList newDict newList


{-| Remove a key-value pair from a `DictList`. If the key is not found,
no changes are made.
-}
remove : comparable -> DictList comparable v -> DictList comparable v
remove key dictList =
    case dictList of
        DictList dict list ->
            if Dict.member key dict then
                -- Lists are not particularly optimized for removals ...
                -- if that becomes a practical issue, we could perhaps
                -- use an `Array` instead.
                DictList
                    (Dict.remove key dict)
                    (List.Extra.remove key list)
            else
                -- We avoid the list removal efficiently in this branch.
                dictList


{-| Update the value for a specific key with a given function. Maintains
the order of the key, or inserts it at the end if it is new.
-}
update : comparable -> (Maybe v -> Maybe v) -> DictList comparable v -> DictList comparable v
update key alter dictList =
    case alter (get key dictList) of
        Nothing ->
            remove key dictList

        Just value ->
            insert key value dictList


{-| Create a `DictList` with one key-value pair.
-}
singleton : comparable -> v -> DictList comparable v
singleton key value =
    DictList (Dict.singleton key value) [ key ]



-- COMBINE


{-| Combine two dictionaries. If keys collide, preference is given
to the value from the first `DictList`.

Keys already in the first `DictList` will remain in their original order.

Keys newly added from the second `DictList` will be added at the end.

So, you might think of `union` as being biased towards the first argument,
since it preserves both key-order and values from the first argument, only
adding things on the right (from the second argument) for keys that were not
present in the first. This seems to correspond best to the logic of `Dict.union`.

For a similar function that is biased towards the second argument, see `append`.
-}
union : DictList comparable v -> DictList comparable v -> DictList comparable v
union t1 t2 =
    foldr cons t2 t1


{-| Keep a key-value pair when its key appears in the second `DictList`.
Preference is given to values in the first `DictList`. The resulting
order of keys will be as it was in the first `DictList`.
-}
intersect : DictList comparable v -> DictList comparable v -> DictList comparable v
intersect t1 t2 =
    filter (\k _ -> member k t2) t1


{-| Keep a key-value pair when its key does not appear in the second `DictList`.
-}
diff : DictList comparable v -> DictList comparable v -> DictList comparable v
diff t1 t2 =
    foldl (\k v t -> remove k t) t1 t2


{-| The most general way of combining two dictionaries. You provide three
accumulators for when a given key appears:

  1. Only in the left `DictList`.
  2. In both dictionaries.
  3. Only in the right `DictList`.

You then traverse all the keys and values, building up whatever
you want.

The keys and values from the first `DictList` will be provided first,
in the order maintained by the first `DictList`. Then, any keys which are
only in the second `DictList` will be provided, in the order maintained
by the second `DictList`.
-}
merge :
    (comparable -> a -> result -> result)
    -> (comparable -> a -> b -> result -> result)
    -> (comparable -> b -> result -> result)
    -> DictList comparable a
    -> DictList comparable b
    -> result
    -> result
merge leftFunc bothFunc rightFunc leftDict (DictList rightDict rightList) initialResult =
    let
        goLeft leftKey leftValue ( remainingRight, accumLeft ) =
            case Dict.get leftKey rightDict of
                Just rightValue ->
                    -- The left key is also in the right dict. So, we remove it
                    -- from the right (since we'll deal with it here) and we
                    -- apply the `bothFunc`
                    ( Dict.remove leftKey remainingRight
                    , bothFunc leftKey leftValue rightValue accumLeft
                    )

                Nothing ->
                    -- The left key is not in the right dict. So, we leave the
                    -- right dict alone, and apply the leftFunc
                    ( remainingRight
                    , leftFunc leftKey leftValue accumLeft
                    )

        goRight remainingRight rightKey accumRight =
            case Dict.get rightKey remainingRight of
                Just rightValue ->
                    -- If we still have one, it means it was only on the right
                    rightFunc rightKey rightValue accumRight

                Nothing ->
                    -- If we don't have it anymore, it was dealt with on the left
                    accumRight
    in
        -- We start on the left, because we have said that the order we'll follow
        -- is left-favouring.
        foldl goLeft ( rightDict, initialResult ) leftDict
            |> (\( remainingRight, accumLeft ) ->
                    -- Now, we go through the right hand side, for those things that
                    -- weren't also on the left.
                    List.foldl (goRight remainingRight) accumLeft rightList
               )



-- TRANSFORM


{-| Apply a function to all values in a `DictList`.
-}
map : (comparable -> a -> b) -> DictList comparable a -> DictList comparable b
map func (DictList dict list) =
    DictList (Dict.map func dict) list


{-| Fold over the key-value pairs in a `DictList`, in order from the first
key to the last key (given the arbitrary order maintained by the `DictList`).
-}
foldl : (comparable -> v -> b -> b) -> b -> DictList comparable v -> b
foldl func accum (DictList dict list) =
    let
        go key acc =
            func key (unsafeGet key dict) acc
    in
        List.foldl go accum list


{-| Fold over the key-value pairs in a `DictList`, in order from the last
key to the first key (given the arbitrary order maintained by the `DictList`.
-}
foldr : (comparable -> v -> b -> b) -> b -> DictList comparable v -> b
foldr func accum (DictList dict list) =
    let
        go key acc =
            case Dict.get key dict of
                Just value ->
                    func key value acc

                Nothing ->
                    Debug.crash "Internal error: DictList list not in sync with dict"
    in
        List.foldr go accum list


{-| Keep a key-value pair when it satisfies a predicate.
-}
filter : (comparable -> v -> Bool) -> DictList comparable v -> DictList comparable v
filter predicate dictList =
    let
        add key value dict =
            if predicate key value then
                insert key value dict
            else
                dict
    in
        foldl add empty dictList


{-| Partition a `DictList` according to a predicate. The first `DictList`
contains all key-value pairs which satisfy the predicate, and the second
contains the rest.
-}
partition : (comparable -> v -> Bool) -> DictList comparable v -> ( DictList comparable v, DictList comparable v )
partition predicate dict =
    let
        add key value ( t1, t2 ) =
            if predicate key value then
                ( insert key value t1, t2 )
            else
                ( t1, insert key value t2 )
    in
        foldl add ( empty, empty ) dict



-- LISTS


{-| Get all of the keys in a `DictList`, in the order maintained by the `DictList`.
-}
keys : DictList comparable v -> List comparable
keys (DictList dict list) =
    list


{-| Get all of the values in a `DictList`, in the order maintained by the `DictList`.
-}
values : DictList comparable v -> List v
values dictList =
    foldr (\key value valueList -> value :: valueList) [] dictList


{-| Convert a `DictList` into an association list of key-value pairs, in the order maintained by the `DictList`.
-}
toList : DictList comparable v -> List ( comparable, v )
toList dict =
    foldr (\key value list -> ( key, value ) :: list) [] dict


{-| Convert an association list into a `DictList`, maintaining the order of the list.
-}
fromList : List ( comparable, v ) -> DictList comparable v
fromList assocs =
    List.foldl (\( key, value ) dict -> insert key value dict) empty assocs


{-| Extract a `Dict` from a `DictList`
-}
toDict : DictList comparable v -> Dict comparable v
toDict (DictList dict list) =
    dict


{-| Given a `Dict`, create a `DictList`. The keys will initially be in the
order that the `Dict` provides.
-}
fromDict : Dict comparable v -> DictList comparable v
fromDict dict =
    DictList dict (Dict.keys dict)



-- LIST EXTRA


{-| Extract the last element of a list.
    last (fromList [(1, 1), (2, 2), (3, 3)]) == Just (3, 3)
    last (fromList []) == Nothing
-}
last : DictList comparable v -> Maybe ( comparable, v )
last xs =
  toList xs
  |> List.Extra.last


{-| Return all initial segments of a list, from shortest to longest, empty list first, the list itself last.

    inits (fromList [(1, 1),(2,),(3, 3)]) == [fromList [], fromList [(1, 1)], fromList [(1, 1), (2, 2)], fromList [(1, 1), (2, 2), (3, 3)]]
-}
inits : DictList comparable v -> List (DictList comparable v)
-- @FIXME
inits list =
  toList list
    |> List.Extra.inits
    |> List.map fromList

{-| Returns a list of repeated applications of `f`.

If `f` returns `Nothing` the iteration will stop. If it returns `Just y` then `y` will be added to the list and the iteration will continue with `f y`.
    nextYear : Int -> Maybe Int
    nextYear year =
      if year >= 2030 then
        Nothing
      else
        Just (year + 1)
    -- Will evaluate to [2010, 2011, ..., 2030]
    iterate nextYear 2010
-}
iterate : ((comparable, v) -> Maybe (comparable, v)) -> (comparable, v) -> DictList comparable v
iterate f x =
  List.Extra.iterate f x
  |> fromList

{-| Decompose a list into its head and tail. If the list is empty, return `Nothing`. Otherwise, return `Just (x, xs)`, where `x` is head and `xs` is tail.

    uncons (fromList [(1, 1),(2, 2),(3, 3)] == Just ((1, 1), fromList [(2, 2), (3, 3)])
    uncons empty = Nothing
-}
uncons : DictList comparable v -> Maybe ( (comparable, v), DictList comparable v)
uncons xs =
  toList xs
    |> List.Extra.uncons
    |> Maybe.map (\(a, la) -> (a, fromList la))

{-| Find the first maximum element in a list using a comparable transformation
-}
maximumBy : (comparable2 -> a -> comparable1) -> DictList comparable2 a -> Maybe (comparable2, a)
maximumBy f ls =
  toList ls
    |> List.Extra.maximumBy (uncurry f)

{-| Find the first minimum element in a list using a comparable transformation
-}
minimumBy : (comparable2 -> a -> comparable1) -> DictList comparable2 a -> Maybe (comparable2, a)
minimumBy f ls =
  toList ls
    |> List.Extra.minimumBy (uncurry f)

{-| Take elements in order as long as the predicate evaluates to `True`
-}
takeWhile : ((comparable, a) -> Bool) -> DictList comparable a -> DictList comparable a
takeWhile predicate xs =
  toList xs
    |> List.Extra.takeWhile predicate
    |> fromList

{-| Drop elements in order as long as the predicate evaluates to `True`
-}
dropWhile : ((comparable, a) -> Bool) -> DictList comparable a -> DictList comparable a
dropWhile predicate list =
  toList list
    |> List.Extra.dropWhile predicate
    |> fromList

{-| Remove duplicate values, keeping the first instance of each element which appears more than once.

    unique [0,1,1,0,1] == [0,1]
-}
unique : DictList comparable1 comparable2 -> DictList comparable1 comparable2
unique list =
  toList list
    |> List.Extra.unique
    |> fromList

{-| Drop duplicates where what is considered to be a duplicate is the result of first applying the supplied function to the elements of the list.
-}
uniqueBy : (comparable1 -> a -> comparable2) -> DictList comparable1 a -> DictList comparable1 a
uniqueBy f list =
  toList list
    |> List.Extra.uniqueBy (uncurry f)
    |> fromList

{-| Indicate if list has duplicate values.

    allDifferent [0,1,1,0,1] == False
-}
allDifferent : DictList comparable1 comparable2 -> Bool
allDifferent list =
  -- @TODO Should this method just use the values, or also the keys
  values list
   |> List.Extra.allDifferent 

{-| Indicate if list has duplicate values when supplied function are applyed on each values.
-}
allDifferentBy : (comparable1 -> a -> comparable2) -> DictList comparable1 a -> Bool
allDifferentBy f list = 
  toList list
    |> List.Extra.allDifferentBy (uncurry f)

{-| Map functions taking multiple arguments over multiple lists. Each list should be of the same length.

    ((\a b c -> a + b * c)
        |> flip map [1,2,3]
        |> andMap [4,5,6]
        |> andMap [2,1,1]
    ) == [9,7,9]
-}
andMap : DictList comparable a -> DictList comparable (a -> b) -> DictList comparable b
andMap l fl =
  let
    keyList = keys l
    lList = values l
    flList = values fl
  in
      List.Extra.andMap lList flList
      |> List.Extra.zip keyList
      |> fromList

{-| Equivalent to `concatMap`. For example, suppose you want to have a cartesian product of [1,2] and [3,4]:

    [1,2] |> andThen (\x -> [3,4]
          |> andThen (\y -> [(x,y)]))

will give back the list:

    [(1,3),(1,4),(2,3),(2,4)]

Now suppose we want to have a cartesian product between the first list and the second list and its doubles:

    [1,2] |> andThen (\x -> [3,4]
          |> andThen (\y -> [y,y*2]
          |> andThen (\z -> [(x,z)])))

will give back the list:

    [(1,3),(1,6),(1,4),(1,8),(2,3),(2,6),(2,4),(2,8)]

Advanced functional programmers will recognize this as the implementation of bind operator (>>=) for lists from the `Monad` typeclass.
-}
andThen : (comparable -> a -> DictList comparable b) -> DictList comparable a -> DictList comparable b
andThen =
    concatMap

concatMap : (comparable -> a -> DictList comparable b) -> DictList comparable a -> DictList comparable b
concatMap f xs = empty
--  map f xs
--    |> toList
--    |> values
--    |> concat

-----------
-- Internal
-----------


{-| For cases where we know the key must be in the `Dict`.
-}
unsafeGet : comparable -> Dict comparable value -> value
unsafeGet key dict =
    case Dict.get key dict of
        Just value ->
            value

        Nothing ->
            Debug.crash "Internal error: DictList list not in sync with dict"
