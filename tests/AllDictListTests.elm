module AllDictListTests exposing (..)

import AllDict
import AllDictList exposing (..)
import Dict
import Expect
import Fuzz exposing (Fuzzer, intRange)
import Json.Encode exposing (Value)
import Set
import Test exposing (Test, describe, fuzz, fuzz2, test)


type Action
    = Run
    | Hide
    | StandStill


ord : Action -> Int
ord action =
    case action of
        Run ->
            2

        Hide ->
            1

        StandStill ->
            0


{-| You wouldn't expect to get an `ord` function like this, but it's actually quite
legitimate. It's what you would need for types that don't have a reliable `==` ...
that is, for types where distinct internal representations ought to be considered
equal ... which is perfectly legitimate.
-}
ordWithUnreliableEquality : Action -> Int
ordWithUnreliableEquality action =
    case action of
        Run ->
            1

        Hide ->
            1

        StandStill ->
            0


actionDict : AllDictList Action String Int
actionDict =
    AllDictList.fromList ord
        [ ( Run, "Run away!" )
        , ( Hide, "Coward!" )
        , ( StandStill, "Err..." )
        ]


basicTest : Test
basicTest =
    test "get" <|
        \_ ->
            AllDictList.get Run actionDict
                |> Expect.equal (Just "Run away!")


thing : String
thing =
    "thing"


insertBeforeTest : Test
insertBeforeTest =
    describe "insertBefore with custom comparator"
        [ describe "with reliable equality"
            [ test "when empty" <|
                \_ ->
                    AllDictList.empty ord
                        |> AllDictList.insertBefore Run Hide thing
                        |> Expect.all
                            [ AllDictList.values >> List.length >> Expect.equal 1
                            , AllDictList.toAllDict >> AllDict.size >> Expect.equal 1
                            , AllDictList.keys >> Expect.equal [ Hide ]
                            ]
            , test "when inserting before existing key" <|
                \_ ->
                    AllDictList.empty ord
                        |> AllDictList.cons Run thing
                        |> AllDictList.insertBefore Run StandStill thing
                        |> Expect.all
                            [ AllDictList.values >> List.length >> Expect.equal 2
                            , AllDictList.toAllDict >> AllDict.size >> Expect.equal 2
                            , AllDictList.keys >> Expect.equal [ StandStill, Run ]
                            ]
            , test "when inserting before non-existing key" <|
                \_ ->
                    AllDictList.empty ord
                        |> AllDictList.cons Run thing
                        |> AllDictList.insertBefore Hide StandStill thing
                        |> Expect.all
                            [ AllDictList.values >> List.length >> Expect.equal 2
                            , AllDictList.toAllDict >> AllDict.size >> Expect.equal 2
                            , AllDictList.keys >> Expect.equal [ StandStill, Run ]
                            ]
            , test "when replacing before existing key" <|
                \_ ->
                    AllDictList.empty ord
                        |> AllDictList.cons Hide thing
                        |> AllDictList.cons Run thing
                        |> AllDictList.insertBefore Run Hide thing
                        |> Expect.all
                            [ AllDictList.values >> List.length >> Expect.equal 2
                            , AllDictList.toAllDict >> AllDict.size >> Expect.equal 2
                            , AllDictList.keys >> Expect.equal [ Hide, Run ]
                            ]
            , test "when replacing before non-existing key" <|
                \_ ->
                    AllDictList.empty ord
                        |> AllDictList.cons Hide thing
                        |> AllDictList.cons Run thing
                        |> AllDictList.insertBefore StandStill Hide thing
                        |> Expect.all
                            [ AllDictList.values >> List.length >> Expect.equal 2
                            , AllDictList.toAllDict >> AllDict.size >> Expect.equal 2
                            , AllDictList.keys >> Expect.equal [ Hide, Run ]
                            ]
            ]
        , describe "with unreliable equality"
            [ test "when empty" <|
                \_ ->
                    AllDictList.empty ordWithUnreliableEquality
                        |> AllDictList.insertBefore Run Hide thing
                        |> Expect.all
                            [ AllDictList.values >> List.length >> Expect.equal 1
                            , AllDictList.toAllDict >> AllDict.size >> Expect.equal 1
                            , AllDictList.keys >> Expect.equal [ Hide ]
                            ]
            , test "when inserting before existing key" <|
                \_ ->
                    AllDictList.empty ordWithUnreliableEquality
                        |> AllDictList.cons Run thing
                        |> AllDictList.insertBefore Run StandStill thing
                        |> Expect.all
                            [ AllDictList.values >> List.length >> Expect.equal 2
                            , AllDictList.toAllDict >> AllDict.size >> Expect.equal 2
                            , AllDictList.keys >> Expect.equal [ StandStill, Run ]
                            ]
            , test "when inserting before non-existing key" <|
                \_ ->
                    AllDictList.empty ordWithUnreliableEquality
                        |> AllDictList.cons Run thing
                        |> AllDictList.insertBefore Hide StandStill thing
                        |> Expect.all
                            [ AllDictList.values >> List.length >> Expect.equal 2
                            , AllDictList.toAllDict >> AllDict.size >> Expect.equal 2
                            , AllDictList.keys >> Expect.equal [ StandStill, Run ]
                            ]
            , test "when replacing before existing key" <|
                \_ ->
                    AllDictList.empty ordWithUnreliableEquality
                        |> AllDictList.cons Hide thing
                        |> AllDictList.cons StandStill thing
                        |> AllDictList.insertBefore StandStill Hide thing
                        |> Expect.all
                            [ AllDictList.values >> List.length >> Expect.equal 2
                            , AllDictList.toAllDict >> AllDict.size >> Expect.equal 2
                            , AllDictList.keys >> Expect.equal [ Hide, StandStill ]
                            ]
            , test "when replacing before non-existing key" <|
                \_ ->
                    AllDictList.empty ordWithUnreliableEquality
                        |> AllDictList.cons Hide thing
                        |> AllDictList.insertBefore StandStill StandStill thing
                        |> Expect.all
                            [ AllDictList.values >> List.length >> Expect.equal 2
                            , AllDictList.toAllDict >> AllDict.size >> Expect.equal 2
                            , AllDictList.keys >> Expect.equal [ StandStill, Hide ]
                            ]
            ]
        ]
