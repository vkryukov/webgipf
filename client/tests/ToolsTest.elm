module ToolsTest exposing (..)

import Expect
import Gipf exposing (..)
import Test exposing (..)
import Tools exposing (..)


largestPrefixTest : Test
largestPrefixTest =
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


maybeListTest : Test
maybeListTest =
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


isSubsetTest : Test
isSubsetTest =
    describe "isSubsetOf tests"
        [ test "All elements of subset are in the list" <|
            \_ ->
                let
                    subset =
                        [ 3, 2, 1 ]

                    list =
                        [ 5, 2, 3, 4, 1 ]
                in
                Expect.equal (isSubsetOf subset list) True
        , test "Not all elements of subset are in the list" <|
            \_ ->
                let
                    subset =
                        [ 1, 2, 6 ]

                    list =
                        [ 1, 2, 3, 4, 5 ]
                in
                Expect.equal (isSubsetOf subset list) False
        ]


removeElementsTest : Test
removeElementsTest =
    describe "removeElements tests"
        [ test "Elements of toRemove are removed from the list" <|
            \_ ->
                let
                    toRemove =
                        [ 1, 3, 5 ]

                    list =
                        [ 1, 2, 3, 4, 5 ]

                    expected =
                        [ 2, 4 ]
                in
                Expect.equal (removeElements toRemove list) expected
        , test "List remains the same when toRemove is empty" <|
            \_ ->
                let
                    toRemove =
                        []

                    list =
                        [ 1, 2, 3, 4, 5 ]
                in
                Expect.equal (removeElements toRemove list) list
        ]


removeElementsFromOneOfSupersetsTest : Test
removeElementsFromOneOfSupersetsTest =
    describe "removeElementsFromOneOfSupersets tests"
        [ test "Elements of list are removed from the first matching superset" <|
            \_ ->
                let
                    supersets =
                        [ [ 4, 5, 6 ], [ 1, 2, 3 ], [ 7, 8, 9 ] ]

                    list =
                        [ 3, 1 ]

                    expected =
                        [ 2 ]
                in
                Expect.equal (removeElementsFromOneOfSupersets supersets list) expected
        , test "Empty list is returned when no superset matches the list" <|
            \_ ->
                let
                    supersets =
                        [ [ 1, 2, 3 ], [ 4, 5, 6 ], [ 7, 8, 9 ] ]

                    list =
                        [ 1, 2, 4 ]
                in
                Expect.equal (removeElementsFromOneOfSupersets supersets list) []
        ]