port module Main exposing (..)

import DictTests
import DictListTests
import Json.Encode exposing (Value)
import ListTests
import ListExtraTests
import Test exposing (..)
import Test.Runner.Node exposing (run)


port emit : ( String, Value ) -> Cmd msg


main =
    run emit all


all : Test
all =
    describe "All tests"
        [ DictListTests.tests
        , DictTests.tests
        , ListTests.tests
        , ListExtraTests.tests
        ]
