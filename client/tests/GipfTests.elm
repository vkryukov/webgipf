module GipfTests exposing (..)

import Expect
import Gipf
import Test exposing (..)


all : Test
all =
    describe "Gipf Tests"
        [ describe "boardPointQ function"
            [ test "checks a point within the board" <|
                \_ ->
                    Expect.equal True (Gipf.boardPointQ { x = 4, y = 4 })
            , test "checks a point outside the board" <|
                \_ ->
                    Expect.equal False (Gipf.boardPointQ { x = 9, y = 9 })
            ]
        , describe "interiorBoardPointQ function"
            [ test "checks an interior point" <|
                \_ ->
                    Expect.equal True (Gipf.interiorBoardPointQ { x = 4, y = 4 })
            , test "checks a non-interior point" <|
                \_ ->
                    Expect.equal False (Gipf.interiorBoardPointQ { x = 0, y = 0 })
            ]
        , describe "edgeBoardPointQ function"
            [ test "checks an edge point" <|
                \_ ->
                    Expect.equal True (Gipf.edgeBoardPointQ { x = 0, y = 4 })
            , test "checks a non-edge point" <|
                \_ ->
                    Expect.equal False (Gipf.edgeBoardPointQ { x = 4, y = 4 })
            ]
        , describe "neighbors function"
            [ test "finds neighbors of a point" <|
                \_ ->
                    let
                        expectedNeighbors =
                            [ { x = 4, y = 5 }
                            , { x = 4, y = 3 }
                            , { x = 5, y = 4 }
                            , { x = 3, y = 4 }
                            , { x = 5, y = 5 }
                            , { x = 3, y = 3 }
                            ]
                    in
                    Expect.equal expectedNeighbors (Gipf.neighbors { x = 4, y = 4 })
            ]
        ]
