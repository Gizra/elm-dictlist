module DictExtraTests exposing (tests)

import Fuzz exposing (Fuzzer)
import DictList exposing (..)
import Expect
import Test exposing (..)
import Set


{-| Fuzz a DictList, given a fuzzer for the keys and values.
-}
fuzzDictList : Fuzzer comparable -> Fuzzer value -> Fuzzer (DictList comparable value)
fuzzDictList fuzzKey fuzzValue =
    Fuzz.tuple ( fuzzKey, fuzzValue )
        |> Fuzz.list
        |> Fuzz.map DictList.fromList


fuzzIntDictList : Fuzzer (DictList Int Int)
fuzzIntDictList =
    fuzzDictList Fuzz.int Fuzz.int


threeElementList =
    (fromList [ ( 1, 1 ), ( 2, 2 ), ( 3, 3 ) ])


tests =
    describe "List Tests"
        [ dictExtraUnitTests
        , dictExtraFuzzTests
        ]


dictExtraUnitTests : Test
dictExtraUnitTests =
    describe "Dict Extra Unittests"
        [ describe "groupBy"
            [ test "empty" <| \() -> Expect.equal (groupBy (\k v -> k) empty) empty
            , test "always equal elements" <| \() -> Expect.equal (groupBy (\k v -> 1) (fromList [ ( 1, 1 ), ( 2, 2 ), ( 3, 3 ) ])) (fromList [ ( 1, [ 1, 2, 3 ] ) ])
            , test "map to original key" <| \() -> Expect.equal (groupBy (\k v -> k) (fromList [ ( 1, 1 ), ( 2, 2 ), ( 3, 3 ) ])) (fromList [ ( 3, [ 3 ] ), ( 2, [ 2 ] ), ( 1, [ 1 ] ) ])
            , test "odd-even" <| \() -> Expect.equal (groupBy (\k v -> k % 2) (fromList [ ( 1, 1 ), ( 2, 2 ), ( 3, 3 ) ])) (fromList [ ( 1, [ 1, 3 ] ), ( 0, [ 2 ] ) ])
            ]
        , describe "fromListBy"
            [ test "empty" <| \() -> Expect.equal (fromListBy (\k v -> k) empty) empty
            , test "simple list" <| \() -> Expect.equal (fromListBy (\k v -> k + 1) (fromList [ ( 1, 1 ), ( 2, 2 ), ( 3, 3 ) ])) (fromList [ ( 2, 1 ), ( 3, 2 ), ( 4, 3 ) ])
            ]
        , describe "removeWhen"
            [ test "empty" <| \() -> Expect.equal (removeWhen (\k v -> True) empty) empty
            , test "remove all" <| \() -> Expect.equal (removeWhen (\k v -> True) (fromList [ ( 1, 1 ), ( 2, 2 ), ( 3, 3 ) ])) empty
            , test "remove none" <| \() -> Expect.equal (removeWhen (\k v -> False) (fromList [ ( 1, 1 ), ( 2, 2 ), ( 3, 3 ) ])) (fromList [ ( 1, 1 ), ( 2, 2 ), ( 3, 3 ) ])
            ]
        , describe "removeMany"
            [ test "empty" <| \() -> Expect.equal (removeMany (Set.fromList [ 1, 2 ]) empty) empty
            , test "remove none element" <| \() -> Expect.equal (removeMany (Set.fromList [ 4 ]) threeElementList) threeElementList
            , test "remove one element" <| \() -> Expect.equal (removeMany (Set.fromList [ 1 ]) threeElementList) (DictList.filter (\k v -> k /= 1) threeElementList)
            , test "remove two elements" <| \() -> Expect.equal (removeMany (Set.fromList [ 1, 2 ]) threeElementList) (DictList.filter (\k v -> k == 3) threeElementList)
            , test "remove all elements" <| \() -> Expect.equal (removeMany (Set.fromList [ 1, 2, 3 ]) threeElementList) empty
            ]
        , describe "keepOnly"
            [ test "empty" <| \() -> Expect.equal (keepOnly (Set.fromList [ 1, 2 ]) empty) empty
            , test "keep none element" <| \() -> Expect.equal (removeMany (Set.fromList [ 4 ]) threeElementList) threeElementList
            , test "keep one element" <| \() -> Expect.equal (keepOnly (Set.fromList [ 1 ]) threeElementList) (DictList.filter (\k v -> k == 1) threeElementList)
            , test "keep two elements" <| \() -> Expect.equal (keepOnly (Set.fromList [ 1, 2 ]) threeElementList) (DictList.filter (\k v -> k /= 3) threeElementList)
            , test "keep all elements" <| \() -> Expect.equal (keepOnly (Set.fromList [ 1, 2, 3 ]) threeElementList) threeElementList
            ]
        , describe "mapKeys"
            [ test "empty" <| \() -> Expect.equal (mapKeys toString empty) empty
            , test "toString mapping" <| \() -> Expect.equal (mapKeys toString threeElementList) (fromList [ ( "1", 1 ), ( "2", 2 ), ( "3", 3 ) ])
            ]
        ]


dictExtraFuzzTests : Test
dictExtraFuzzTests =
    -- @TODO Expand the fuzz tests
    describe "Dict extra fuzz tests"
        [ fuzz fuzzIntDictList "groupBy (total length doesn't change)" <|
            \subject ->
                Expect.equal (DictList.length subject)
                    (groupBy (\k v -> k % 2) subject
                        |> toList
                        |> List.map (\( k, v ) -> List.length v)
                        |> List.foldr (+) 0
                    )
        , fuzz fuzzIntDictList "groupBy (no elements dissapear)" <|
            \subject ->
                Expect.equal
                    (Set.diff (Set.fromList (values subject))
                        (Set.fromList
                            (groupBy (\k v -> k % 2) subject
                                |> toList
                                |> List.foldr (\( k, v ) agg -> List.append v agg) []
                            )
                        )
                    )
                    (Set.fromList [])
        ]
