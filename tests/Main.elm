port module Main exposing (..)

import DictTests
import Json.Encode exposing (Value)
import Test exposing (..)
import Test.Runner.Node exposing (run)


port emit : ( String, Value ) -> Cmd msg


main =
    run emit all


all : Test
all =
    describe "All tests"
        [ DictTests.all
        ]
