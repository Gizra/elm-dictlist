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
        , next
        , previous
          -- JSON
        , decodeObject
        , decodeWithKeys
        , decodeArray
          -- Conversion
        , toDict
        )

{-| Have you ever wanted a `Dict`, but you need to maintain an arbitrary
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

# DictList
@docs DictList

# Build
@docs empty, singleton, insert, update, remove

# Query
@docs isEmpty, member, get, size

#Convert
@docs keys, values, toList, fromList, toDict

# Lists

For list-like manipulations which are not exposed yet, you can use `keys`,
`values`, or `toList` to get a list, and then possibly `fromList` to create
a modified `DictList`.

@docs cons, head, tail, indexedMap, filterMap, length, reverse, all, any, append, concat
@docs sum, product, maximum, minimum, take, drop, sort, sortBy, sortWith
@docs getAt, getKeyAt, indexOfKey
@docs insertAfter, next, previous

# Transform
@docs map, foldl, foldr, filter, partition

# Combine
@docs union, intersect, diff, merge

# JSON
@docs decodeObject, decodeArray, decodeWithKeys
-}

import Dict exposing (Dict)
import DictList.Compat exposing (customDecoder, first, maybeAndThen, second)
import Json.Decode exposing (Decoder, keyValuePairs, value, decodeValue)
import List.Extra


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
--
-- We'll gradually add more list-oriented functions as we figure out what we
-- need. In the meantime, one generic alternative is to use `keys`, `values` or
-- `toList` to get a list, run `List` functions on that, and then possibly
-- `fromList` to recreate a modified `DictList`. But we can often be more
-- efficient than that by writing optimized versions of list-like functions
-- here.


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


{-| Extract the rest of the keys, with their values.
-}
tail : DictList comparable value -> Maybe (List ( comparable, value ))
tail (DictList dict list) =
    List.tail list
        |> Maybe.map (List.map (\key -> ( key, unsafeGet key dict )))


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
        foldr go ( 0, empty ) >> second


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

* If keys collide, preference is given to the value from the first `DictList`.

* Keys already in the first `DictList` will remain in their original order.

* Keys newly added from the second `DictList` will be added at the end.
-}
append : DictList comparable value -> DictList comparable value -> DictList comparable value
append =
    union


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


{-| Gets the key and value at the next position.
-}
next : comparable -> DictList comparable value -> Maybe ( comparable, value )
next key dictlist =
    indexOfKey key dictlist
        |> maybeAndThen (\index -> getAt (index + 1) dictlist)


{-| Gets the key and value at the previous position.
-}
previous : comparable -> DictList comparable value -> Maybe ( comparable, value )
previous key dictlist =
    indexOfKey key dictlist
        |> maybeAndThen (\index -> getAt (index - 1) dictlist)


{-| Gets the key at the specified index.
-}
getKeyAt : Int -> DictList key value -> Maybe key
getKeyAt index (DictList dict list) =
    List.Extra.getAt index list


{-| Gets the key and value at the specified index.
-}
getAt : Int -> DictList comparable value -> Maybe ( comparable, value )
getAt index (DictList dict list) =
    List.Extra.getAt index list
        |> maybeAndThen
            (\key ->
                Dict.get key dict
                    |> Maybe.map (\value -> ( key, value ))
            )


{-| Insert a key-value pair into a `DictList`, replacing an existing value if
the keys collide. The key will be inserted after the first parameter (whether
or not the key already exists). If the first parameter cannot be found, the
key will be inserted at the end.
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


{-| Fold over the key-value pairs in a `DictList`, in order from lowest
key to highest key (given the arbitrary order maintained by the `DictList`).
-}
foldl : (comparable -> v -> b -> b) -> b -> DictList comparable v -> b
foldl func accum (DictList dict list) =
    let
        go key acc =
            func key (unsafeGet key dict) acc
    in
        List.foldl go accum list


{-| Fold over the key-value pairs in a `DictList`, in order from highest
key to lowest key (given the arbitrary order maintained by the `DictList`.
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
