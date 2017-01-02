module DictTests exposing (all)

{-| This is an adaptation of the Elm core tests for `Dict`. The idea is
to test whether we're a good `Dict` or not.

We import `DictList` as `Dict` to limit the code modifications below
-}

import DictList as Dict exposing (DictList)
import Expect exposing (Expectation)
import Test exposing (..)


all : Test
all =
    describe "Dict tests"
        [ buildTests
        , queryTests
        , combineTests
        , transformTests
        , mergeTests
        ]


animals : DictList String String
animals =
    Dict.fromList [ ( "Tom", "cat" ), ( "Jerry", "mouse" ) ]


{-| This adapts the older elm-test to the newer version.
-}
assertEqual : a -> a -> (() -> Expectation)
assertEqual value expected =
    \_ -> Expect.equal expected value


buildTests : Test
buildTests =
    describe "build Tests"
        [ test "empty" <| assertEqual (Dict.fromList []) (Dict.empty)
        , test "singleton" <| assertEqual (Dict.fromList [ ( "k", "v" ) ]) (Dict.singleton "k" "v")
        , test "insert" <| assertEqual (Dict.fromList [ ( "k", "v" ) ]) (Dict.insert "k" "v" Dict.empty)
        , test "insert replace" <| assertEqual (Dict.fromList [ ( "k", "vv" ) ]) (Dict.insert "k" "vv" (Dict.singleton "k" "v"))
        , test "update" <| assertEqual (Dict.fromList [ ( "k", "vv" ) ]) (Dict.update "k" (\v -> Just "vv") (Dict.singleton "k" "v"))
        , test "update Nothing" <| assertEqual Dict.empty (Dict.update "k" (\v -> Nothing) (Dict.singleton "k" "v"))
        , test "remove" <| assertEqual Dict.empty (Dict.remove "k" (Dict.singleton "k" "v"))
        , test "remove not found" <| assertEqual (Dict.singleton "k" "v") (Dict.remove "kk" (Dict.singleton "k" "v"))
        ]


queryTests : Test
queryTests =
    describe "query Tests"
        [ test "member 1" <| assertEqual True (Dict.member "Tom" animals)
        , test "member 2" <| assertEqual False (Dict.member "Spike" animals)
        , test "get 1" <| assertEqual (Just "cat") (Dict.get "Tom" animals)
        , test "get 2" <| assertEqual Nothing (Dict.get "Spike" animals)
        , test "size of empty dictionary" <| assertEqual 0 (Dict.size Dict.empty)
        , test "size of example dictionary" <| assertEqual 2 (Dict.size animals)
        ]


combineTests : Test
combineTests =
    describe "combine Tests"
        [ test "union" <| assertEqual animals (Dict.union (Dict.singleton "Jerry" "mouse") (Dict.singleton "Tom" "cat"))
        , test "union collison" <| assertEqual (Dict.singleton "Tom" "cat") (Dict.union (Dict.singleton "Tom" "cat") (Dict.singleton "Tom" "mouse"))
        , test "intersect" <| assertEqual (Dict.singleton "Tom" "cat") (Dict.intersect animals (Dict.singleton "Tom" "cat"))
        , test "diff" <| assertEqual (Dict.singleton "Jerry" "mouse") (Dict.diff animals (Dict.singleton "Tom" "cat"))
        ]


transformTests : Test
transformTests =
    describe "transform Tests"
        [ test "filter" <| assertEqual (Dict.singleton "Tom" "cat") (Dict.filter (\k v -> k == "Tom") animals)
        , test "partition" <| assertEqual ( Dict.singleton "Tom" "cat", Dict.singleton "Jerry" "mouse" ) (Dict.partition (\k v -> k == "Tom") animals)
        ]


mergeTests : Test
mergeTests =
    let
        insertBoth key leftVal rightVal dict =
            Dict.insert key (leftVal ++ rightVal) dict

        s1 =
            Dict.empty |> Dict.insert "u1" [ 1 ]

        s2 =
            Dict.empty |> Dict.insert "u2" [ 2 ]

        s23 =
            Dict.empty |> Dict.insert "u2" [ 3 ]

        b1 =
            List.map (\i -> ( i, [ i ] )) [1..10] |> Dict.fromList

        b2 =
            List.map (\i -> ( i, [ i ] )) [5..15] |> Dict.fromList

        bExpected =
            [ ( 1, [ 1 ] ), ( 2, [ 2 ] ), ( 3, [ 3 ] ), ( 4, [ 4 ] ), ( 5, [ 5, 5 ] ), ( 6, [ 6, 6 ] ), ( 7, [ 7, 7 ] ), ( 8, [ 8, 8 ] ), ( 9, [ 9, 9 ] ), ( 10, [ 10, 10 ] ), ( 11, [ 11 ] ), ( 12, [ 12 ] ), ( 13, [ 13 ] ), ( 14, [ 14 ] ), ( 15, [ 15 ] ) ]
    in
        describe "merge Tests"
            [ test "merge empties" <|
                assertEqual (Dict.empty)
                    (Dict.merge Dict.insert insertBoth Dict.insert Dict.empty Dict.empty Dict.empty)
            , test "merge singletons in order" <|
                assertEqual [ ( "u1", [ 1 ] ), ( "u2", [ 2 ] ) ]
                    ((Dict.merge Dict.insert insertBoth Dict.insert s1 s2 Dict.empty) |> Dict.toList)
            , test "merge singletons out of order" <|
                -- This one behaves differently in DictList than Dict, since DictList preserves arbitrary order
                assertEqual [ ( "u2", [ 2 ] ), ( "u1", [ 1 ] ) ]
                    ((Dict.merge Dict.insert insertBoth Dict.insert s2 s1 Dict.empty) |> Dict.toList)
            , test "merge with duplicate key" <|
                assertEqual [ ( "u2", [ 2, 3 ] ) ]
                    ((Dict.merge Dict.insert insertBoth Dict.insert s2 s23 Dict.empty) |> Dict.toList)
            , test "partially overlapping" <|
                assertEqual bExpected
                    ((Dict.merge Dict.insert insertBoth Dict.insert b1 b2 Dict.empty) |> Dict.toList)
            ]
