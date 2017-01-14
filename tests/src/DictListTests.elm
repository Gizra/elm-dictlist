module DictListTests exposing (tests)

{-| These are tests of specifically `DictList` behaviour ... that is,
things not necessarily tested by the `DictTests` or the `ListTests`.
-}

import DictList exposing (DictList)
import Expect
import Fuzz exposing (Fuzzer)
import Json.Decode exposing (Decoder, field)
import Result exposing (Result(..))
import Test exposing (..)


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
    Json.Decode.map2 ValueWithId
        (field "id" Json.Decode.int)
        (field "value" Json.Decode.int)


jsonTests : Test
jsonTests =
    describe "JSON tests"
        [ fuzz jsonObjectAndExpectedResult "decodeObject gets the expected dict (not necessarily order)" <|
            \( json, expected ) ->
                json
                    |> Json.Decode.decodeString (DictList.decodeObject Json.Decode.int)
                    |> Result.map DictList.toDict
                    |> Expect.equal (Ok (DictList.toDict expected))
        , fuzz jsonArrayAndExpectedResult "decodeArray preserves order" <|
            \( json, expected ) ->
                json
                    |> Json.Decode.decodeString (DictList.decodeArray .id decodeValueWithId)
                    |> Expect.equal (Ok expected)
        ]


tests : Test
tests =
    describe "DictList tests"
        [ jsonTests
        ]
