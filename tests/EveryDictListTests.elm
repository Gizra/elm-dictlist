module EveryDictListTests exposing (Action(..), actionDict, basicTest)

import Dict
import EveryDictList exposing (..)
import Expect
import Fuzz exposing (Fuzzer, intRange)
import Json.Encode exposing (Value)
import Set
import Test exposing (Test, describe, fuzz, fuzz2, test)


type Action
    = Run
    | Hide
    | StandStill


actionDict : EveryDictList Action String
actionDict =
    EveryDictList.fromList
        [ ( Run, "Run away!" )
        , ( Hide, "Coward!" )
        , ( StandStill, "Err..." )
        ]


basicTest : Test
basicTest =
    test "get" <|
        \_ ->
            EveryDictList.get Run actionDict
                |> Expect.equal (Just "Run away!")
