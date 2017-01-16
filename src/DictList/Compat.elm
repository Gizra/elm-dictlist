module DictList.Compat
    exposing
        ( customDecoder
        , first
        , maybeAndThen
        , second
        )

{-| This module consists of functions used by `DictList` which need to
be different in Elm 0.17 and 0.18. This allows us to easily maintain
an internal version of `DictList` for use in Elm 0.17 projects (importing
a different version of this file), while publishing an Elm 0.18 version.
-}

import Json.Decode exposing (Decoder, succeed, fail)


{-| The Elm 0.18 signature for `Maybe.andThen` (incompatible with Elm 0.17's
version).
-}
maybeAndThen : (a -> Maybe b) -> Maybe a -> Maybe b
maybeAndThen =
    Maybe.andThen


{-| The Elm 0.17 signature for `Json.Decode.customDecoder` (not present in Elm 0.18).
-}
customDecoder : Decoder a -> (a -> Result String b) -> Decoder b
customDecoder decoder toResult =
    decoder
        |> Json.Decode.andThen
            (\a ->
                case toResult a of
                    Ok b ->
                        succeed b

                    Err err ->
                        fail err
            )


{-| The Elm 0.18 version of Tuple.first
-}
first : ( a, b ) -> a
first =
    Tuple.first


{-| The Elm 0.18 version of Tuple.second
-}
second : ( a, b ) -> b
second =
    Tuple.second
