module ToolsTest exposing (..)

import Expect
import Gipf exposing (..)
import Test exposing (..)
import Tools exposing (..)


toolsTest : Test
toolsTest =
    describe "Tools tests"
        [ describe "largestPrefixWithoutNothihg function"
            [ test "finds the largest prefix without Nothing, starting with a Just item" <|
                \_ ->
                    Expect.equal [ 1, 2, 3 ] (largestPrefixWithoutNothing [ Just 1, Just 2, Just 3, Nothing, Just 4, Just 5 ])
            , test "finds the largest prefix without Nothing, starting with a Nothing item" <|
                \_ ->
                    Expect.equal [] (largestPrefixWithoutNothing [ Nothing, Just 1, Just 2, Just 3, Nothing, Just 4, Just 5 ])
            , test "finds the largest prefix without Nothing when it's emmpty" <|
                \_ ->
                    Expect.equal [] (largestPrefixWithoutNothing [ Nothing, Just 1, Just 2, Just 3, Nothing, Just 4, Just 5 ])
            ]
        , describe "extendSublistWithJustItems function" <|
            [ test "extends a sublist starting with 0" <|
                \_ ->
                    Expect.equal [ 1, 2, 3, 4 ]
                        (extendSublistWithJustItems [ Just 1, Just 2, Just 3, Just 4, Nothing, Just 5 ] 0)
            , test "extends a sublist starting with 2 but no left" <|
                \_ ->
                    Expect.equal [ 1, 2, 3, 4 ]
                        (extendSublistWithJustItems [ Just 0, Nothing, Just 1, Just 2, Just 3, Just 4, Nothing, Just 5 ] 2)
            , test "extends a sublist starting with 2" <|
                \_ ->
                    Expect.equal [ -1, 1, 2, 3, 4 ]
                        (extendSublistWithJustItems [ Just 0, Nothing, Just -1, Just 1, Just 2, Just 3, Just 4, Nothing, Just 5 ] 2)
            ]
        ]


tests : Test
tests =
    describe "maybeList function"
        [ test "All Just values" <|
            \_ ->
                let
                    input =
                        [ Just 1, Just 2, Just 3 ]

                    expected =
                        Just [ 1, 2, 3 ]
                in
                Expect.equal (maybeList input) expected
        , test "One Nothing value" <|
            \_ ->
                let
                    input =
                        [ Just 1, Nothing, Just 3 ]

                    expected =
                        Nothing
                in
                Expect.equal (maybeList input) expected
        ]
