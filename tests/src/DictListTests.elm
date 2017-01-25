module DictListTests exposing (tests)

{-| These are tests of specifically `DictList` behaviour ... that is,
things not necessarily tested by the `DictTests` or the `ListTests`.
-}

import Arithmetic exposing (isEven)
import DictList exposing (DictList)
import Expect
import Fuzz exposing (Fuzzer)
import Json.Decode as JD exposing (Decoder, field)
import List.Extra
import Result exposing (Result(..))
import Test exposing (..)


{-| Fuzz a DictList, given a fuzzer for the keys and values.
-}
fuzzDictList : Fuzzer comparable -> Fuzzer value -> Fuzzer (DictList comparable value)
fuzzDictList fuzzKey fuzzValue =
    Fuzz.tuple ( fuzzKey, fuzzValue )
        |> Fuzz.list
        |> Fuzz.map DictList.fromList


{-| We make our own JSON string because Elm doesn't normally promise
anything about the order of values in a JSON object. So, we make sure
that the order in the JSON string is well-known, so we can test
what happens.

We also reject duplicate keys (since that would be unexpected JSON).

In addition to the JSON string, we return what we would expect from
DictList after the string is decoded.
-}
jsonObjectAndExpectedResult : Fuzzer ( String, DictList String Int )
jsonObjectAndExpectedResult =
    Fuzz.tuple ( Fuzz.int, Fuzz.int )
        |> Fuzz.list
        |> Fuzz.map
            (\list ->
                let
                    go ( key, value ) ( json, expected ) =
                        if DictList.member (toString key) expected then
                            ( json, expected )
                        else
                            ( ("\"" ++ toString key ++ "\": " ++ toString value) :: json
                            , DictList.cons (toString key) value expected
                            )
                in
                    list
                        |> List.foldr go ( [], DictList.empty )
                        |> (\( json, expected ) ->
                                ( "{" ++ String.join ", " json ++ "}"
                                , expected
                                )
                           )
            )


{-| Like the above, but produces a JSON array.
-}
jsonArrayAndExpectedResult : Fuzzer ( String, DictList Int ValueWithId )
jsonArrayAndExpectedResult =
    Fuzz.tuple ( Fuzz.int, Fuzz.int )
        |> Fuzz.list
        |> Fuzz.map
            (\list ->
                let
                    go ( key, value ) ( json, expected ) =
                        if DictList.member key expected then
                            ( json, expected )
                        else
                            ( ("{\"id\": " ++ toString key ++ ", \"value\": " ++ toString value ++ "}") :: json
                            , DictList.cons key (ValueWithId key value) expected
                            )
                in
                    list
                        |> List.foldr go ( [], DictList.empty )
                        |> (\( json, expected ) ->
                                ( "[" ++ String.join ", " json ++ "]"
                                , expected
                                )
                           )
            )


type alias ValueWithId =
    { id : Int
    , value : Int
    }


decodeValueWithId : Decoder ValueWithId
decodeValueWithId =
    JD.map2 ValueWithId
        (field "id" JD.int)
        (field "value" JD.int)


{-| Like `jsonObjectAndExpectedResult`, but the JSON looks like this:

    { keys: [ ]
    , dict: {  }
    }

... that is, we list an array of keys separately, so that we can preserve
order.
-}
jsonKeysObjectAndExpectedResult : Fuzzer ( String, DictList String Int )
jsonKeysObjectAndExpectedResult =
    Fuzz.tuple ( Fuzz.int, Fuzz.int )
        |> Fuzz.list
        |> Fuzz.map
            (\list ->
                let
                    go ( key, value ) ( jsonKeys, jsonDict, expected ) =
                        if DictList.member (toString key) expected then
                            ( jsonKeys, jsonDict, expected )
                        else
                            ( ("\"" ++ toString key ++ "\"") :: jsonKeys
                            , ("\"" ++ toString key ++ "\": " ++ toString value) :: jsonDict
                            , DictList.cons (toString key) value expected
                            )
                in
                    list
                        |> List.foldr go ( [], [], DictList.empty )
                        |> (\( jsonKeys, jsonDict, expected ) ->
                                let
                                    keys =
                                        "\"keys\": [" ++ String.join ", " jsonKeys ++ "]"

                                    dict =
                                        "\"dict\": {" ++ String.join ", " jsonDict ++ "}"
                                in
                                    ( "{" ++ keys ++ ", " ++ dict ++ "}"
                                    , expected
                                    )
                           )
            )


jsonTests : Test
jsonTests =
    describe "JSON tests"
        [ fuzz jsonObjectAndExpectedResult "decodeObject gets the expected dict (not necessarily order)" <|
            \( json, expected ) ->
                json
                    |> JD.decodeString (DictList.decodeObject JD.int)
                    |> Result.map DictList.toDict
                    |> Expect.equal (Ok (DictList.toDict expected))
        , fuzz jsonArrayAndExpectedResult "decodeArray preserves order" <|
            \( json, expected ) ->
                json
                    |> JD.decodeString (DictList.decodeArray .id decodeValueWithId)
                    |> Expect.equal (Ok expected)
        , fuzz jsonKeysObjectAndExpectedResult "decodeWithKeys gets expected result" <|
            \( json, expected ) ->
                let
                    keyDecoder =
                        field "keys" (JD.list JD.string)

                    valueDecoder key =
                        JD.at [ "dict", key ] JD.int
                in
                    json
                        |> JD.decodeString (DictList.decodeKeysAndValues keyDecoder valueDecoder)
                        |> Expect.equal (Ok expected)
        ]


consTest : Test
consTest =
    fuzz3 Fuzz.int
        Fuzz.int
        (fuzzDictList Fuzz.int Fuzz.int)
        "cons"
        (\key value dictList ->
            let
                expectedSize result =
                    DictList.size result
                        |> Expect.equal
                            (if DictList.member key dictList then
                                DictList.size dictList
                             else
                                DictList.size dictList + 1
                            )

                expectedHead result =
                    DictList.head result
                        |> Expect.equal (Just ( key, value ))
            in
                DictList.cons key value dictList
                    |> Expect.all
                        [ expectedSize
                        , expectedHead
                        ]
        )


headTailConsTest : Test
headTailConsTest =
    fuzz (fuzzDictList Fuzz.int Fuzz.int)
        "headTailCons"
        (\subject ->
            let
                run =
                    Maybe.map2 (uncurry DictList.cons)
                        (DictList.head subject)
                        (DictList.tail subject)

                expected =
                    if DictList.size subject == 0 then
                        Nothing
                    else
                        Just subject
            in
                Expect.equal expected run
        )


indexedMapTest : Test
indexedMapTest =
    fuzz (fuzzDictList Fuzz.int Fuzz.int)
        "indexedMap"
        (\subject ->
            let
                go index key value =
                    { index = index
                    , key = key
                    , value = value
                    }

                listIndexes =
                    DictList.keys subject
                        |> List.indexedMap (\index _ -> index)

                expectIndexes values =
                    values
                        |> List.map .index
                        |> Expect.equal listIndexes

                expectKeys values =
                    values
                        |> List.map .key
                        |> Expect.equal (DictList.keys subject)

                expectValues values =
                    values
                        |> List.map .value
                        |> Expect.equal (DictList.values subject)
            in
                DictList.indexedMap go subject
                    |> DictList.values
                    |> Expect.all
                        [ expectIndexes
                        , expectKeys
                        , expectValues
                        ]
        )


filterMapTest : Test
filterMapTest =
    fuzz (fuzzDictList Fuzz.int Fuzz.int)
        "filterMap"
        (Expect.all
            [ \subject ->
                DictList.filterMap (\_ v -> Just v) subject
                    |> Expect.equal subject
            , \subject ->
                DictList.filterMap (\_ v -> Nothing) subject
                    |> Expect.equal DictList.empty
            ]
        )


lengthTest : Test
lengthTest =
    fuzz (fuzzDictList Fuzz.int Fuzz.int)
        "length behaves like List.length"
        (\subject ->
            subject
                |> DictList.length
                |> Expect.equal (DictList.toList subject |> List.length)
        )


reverseTest : Test
reverseTest =
    fuzz (fuzzDictList Fuzz.int Fuzz.int)
        "reverse behaves like List.reverse"
        (\subject ->
            subject
                |> DictList.reverse
                |> DictList.toList
                |> Expect.equal (DictList.toList subject |> List.reverse)
        )


allTest : Test
allTest =
    fuzz (fuzzDictList Fuzz.int Fuzz.int)
        "all behaves like List.all"
        (\subject ->
            subject
                |> DictList.all (\k v -> isEven k && isEven v)
                |> Expect.equal
                    (DictList.toList subject
                        |> List.all (\( k, v ) -> isEven k && isEven v)
                    )
        )


anyTest : Test
anyTest =
    fuzz (fuzzDictList Fuzz.int Fuzz.int)
        "any behaves like List.any"
        (\subject ->
            subject
                |> DictList.any (\k v -> isEven k && isEven v)
                |> Expect.equal
                    (DictList.toList subject
                        |> List.any (\( k, v ) -> isEven k && isEven v)
                    )
        )


unionTest : Test
unionTest =
    fuzz2
        (fuzzDictList Fuzz.int Fuzz.int)
        (fuzzDictList Fuzz.int Fuzz.int)
        "union"
        (\left right ->
            DictList.union left right
                |> Expect.all
                    [ \result ->
                        -- See if we have the expected number of pairs
                        result
                            |> DictList.size
                            |> Expect.equal
                                (left
                                    |> DictList.keys
                                    |> List.append (DictList.keys right)
                                    |> List.Extra.unique
                                    |> List.length
                                )
                    , \result ->
                        -- The first part of the result should be equal to
                        -- the left side of the input, since keys from the left
                        -- remain in the original order, and we prever values from
                        -- the left where there are collisions.
                        result
                            |> DictList.take (DictList.size left)
                            |> Expect.equal left
                    , \result ->
                        -- The rest of the result should equal what was on the right,
                        -- without things which were already in left
                        result
                            |> DictList.drop (DictList.size left)
                            |> Expect.equal (DictList.filter (\k _ -> not (DictList.member k left)) right)
                    ]
        )


appendTest : Test
appendTest =
    fuzz2
        (fuzzDictList Fuzz.int Fuzz.int)
        (fuzzDictList Fuzz.int Fuzz.int)
        "append"
        (\left right ->
            DictList.append left right
                |> Expect.all
                    [ \result ->
                        -- See if we have the expected number of pairs
                        result
                            |> DictList.size
                            |> Expect.equal
                                (left
                                    |> DictList.keys
                                    |> List.append (DictList.keys right)
                                    |> List.Extra.unique
                                    |> List.length
                                )
                    , \result ->
                        -- The last part of the result should be equal to
                        -- the right side of the input, since keys from the right
                        -- remain in the original order, and we prever values from
                        -- the right where there are collisions.
                        result
                            |> DictList.drop (DictList.size result - DictList.size right)
                            |> Expect.equal right
                    , \result ->
                        -- The rest of the result should equal what was on the left,
                        -- without things which were already on the right
                        result
                            |> DictList.take (DictList.size result - DictList.size right)
                            |> Expect.equal (DictList.filter (\k _ -> not (DictList.member k right)) left)
                    ]
        )


tests : Test
tests =
    describe "DictList tests"
        [ jsonTests
        , consTest
        , headTailConsTest
        , indexedMapTest
        , filterMapTest
        , lengthTest
        , reverseTest
        , allTest
        , anyTest
        , appendTest
        , unionTest
        ]
