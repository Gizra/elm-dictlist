module ListExtraTests exposing (tests)

{-| This is an adaptation of the `List` tests in elm-lang/core, in order
to test whether we are a well-behaved list.
-}

import Test exposing (..)
import Expect exposing (Expectation)
import Maybe exposing (Maybe(Nothing, Just))
import DictList exposing (..)
import List.Extra


tests : Test
tests =
    describe "List Tests"
        [ testListOfN 0
        , testListOfN 1
        , testListOfN 2
        , testListOfN 5000
        ]


toDictList : List comparable -> DictList comparable comparable
toDictList =
    List.map (\a -> ( a, a )) >> DictList.fromList


testListOfN : Int -> Test
testListOfN n =
    let
        xs =
            List.range 1 n |> toDictList

        xsOpp =
            List.range -n -1 |> toDictList

        xsNeg =
            foldl cons empty xsOpp

        -- assume foldl and (::) work
        zs =
            List.range 0 n
                |> List.map (\a -> ( a, a ))
                |> fromList

        sumSeq k =
            k * (k + 1) // 2

        xsSum =
            sumSeq n

        mid =
            n // 2
    in
        describe (toString n ++ " elements")
            [ test "last" <|
                \() ->
                    if n == 0 then
                        Expect.equal (Nothing) (last xs)
                    else
                        Expect.equal (Just ( n, n )) (last xs)
            , test "inits" <|
               \() ->
                  if n == 0 then
                    Expect.equal [empty] (inits empty)
                  else
                    Expect.equal [empty, fromList [(1,1)], fromList [(1,1), (2,2)]] (inits (fromList [(1,1), (2,2)]))
           , test "(!!)" <|
              \() ->
                if n == 0 then
                  Expect.equal Nothing ((!!) empty 0)
                else
                  Expect.equal (Just (n, n)) ((!!) xs (n-1))
            , test "uncons" <|
               \() ->
                 if n == 0 then
                  Expect.equal Nothing (uncons empty)
                else
                  -- @TODO Generalize
                  Expect.equal (Just ((1, 1), fromList [(2, 2), (3, 3)])) (uncons (fromList [(1, 1), (2, 2), (3, 3)]))
            ]