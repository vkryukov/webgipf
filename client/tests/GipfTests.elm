module GipfTests exposing (..)

import Expect
import Gipf exposing (Coord, Move, allMoves, boardPointQ, boardPoints, boardSlice, edgeBoardPointQ, interiorBoardPointQ, neighbors)
import List exposing (sortWith)
import Test exposing (..)


compareCoord : Coord -> Coord -> Order
compareCoord coord1 coord2 =
    case compare coord1.x coord2.x of
        EQ ->
            compare coord1.y coord2.y

        order ->
            order


sortCoords : List Coord -> List Coord
sortCoords coords =
    sortWith compareCoord coords


sortedEqual : List Coord -> List Coord -> Expect.Expectation
sortedEqual a b =
    Expect.equal (sortCoords a) (sortCoords b)


neighborsTest : String -> Coord -> List Coord -> Test
neighborsTest description point expectedNeighbors =
    test description <|
        \_ ->
            sortedEqual expectedNeighbors (neighbors point)


boardSliceTest : String -> Coord -> Coord -> List Coord -> Test
boardSliceTest description start end expectedSlice =
    test description <|
        \_ ->
            sortedEqual expectedSlice (boardSlice start end)


board : Test
board =
    describe "Board Tests"
        [ describe "boardPointQ function"
            [ test "checks a point within the board" <|
                \_ ->
                    Expect.equal True (boardPointQ { x = 4, y = 4 })
            , test "checks a point outside the board" <|
                \_ ->
                    Expect.equal False (boardPointQ { x = 9, y = 9 })
            ]
        , describe "interiorBoardPointQ function"
            [ test "checks an interior point" <|
                \_ ->
                    Expect.equal True (interiorBoardPointQ { x = 4, y = 4 })
            , test "checks a non-interior point" <|
                \_ ->
                    Expect.equal False (interiorBoardPointQ { x = 0, y = 0 })
            ]
        , describe "edgeBoardPointQ function"
            [ test "checks an edge point" <|
                \_ ->
                    Expect.equal True (edgeBoardPointQ { x = 0, y = 4 })
            , test "checks a non-edge point" <|
                \_ ->
                    Expect.equal False (edgeBoardPointQ { x = 4, y = 4 })
            ]
        , describe "neighbors function"
            [ neighborsTest "finds neighbors of a point at (4, 4)"
                { x = 4, y = 4 }
                [ { x = 4, y = 5 }
                , { x = 4, y = 3 }
                , { x = 5, y = 4 }
                , { x = 3, y = 4 }
                , { x = 5, y = 5 }
                , { x = 3, y = 3 }
                ]
            , neighborsTest "finds neighbors of a point at (0, 0)"
                { x = 0, y = 0 }
                [ { x = 1, y = 1 }
                ]
            , neighborsTest "finds neighbors of a point at (0, 4)"
                { x = 0, y = 4 }
                [ { x = 1, y = 4 }
                ]
            , neighborsTest "finds neighbors of a point at (1, 0)"
                { x = 1, y = 0 }
                [ { x = 1, y = 1 }
                , { x = 2, y = 1 }
                ]
            , neighborsTest "finds neighbors of a point at (7, 7)"
                { x = 7, y = 7 }
                [ { x = 6, y = 7 }
                , { x = 7, y = 6 }
                , { x = 6, y = 6 }
                ]
            ]
        , describe "boardPoints function"
            [ test "finds all board points" <|
                \_ ->
                    Expect.equal 61 (List.length boardPoints)
            ]
        ]


compareMoves : Move -> Move -> Order
compareMoves move1 move2 =
    case compareCoord move1.from move2.from of
        EQ ->
            compareCoord move1.to move2.to

        order ->
            order


sortMoves : List Move -> List Move
sortMoves moves =
    sortWith compareMoves moves


movesTest : Test
movesTest =
    describe "move tests"
        [ describe "boardSlice function"
            [ boardSliceTest "checks board slice from {2,0} to {6,4}"
                { x = 2, y = 0 }
                { x = 6, y = 4 }
                [ { x = 3, y = 1 }
                , { x = 4, y = 2 }
                , { x = 5, y = 3 }
                , { x = 6, y = 4 }
                , { x = 7, y = 5 }
                ]
            , boardSliceTest "checks board slice from {4,8} to {4,5}"
                { x = 4, y = 8 }
                { x = 4, y = 5 }
                [ { x = 4, y = 1 }
                , { x = 4, y = 2 }
                , { x = 4, y = 3 }
                , { x = 4, y = 4 }
                , { x = 4, y = 5 }
                , { x = 4, y = 6 }
                , { x = 4, y = 7 }
                ]
            ]
        , describe "available moves"
            [ test "allMoves contains 42 elements" <|
                \_ ->
                    Expect.equal 42 (List.length allMoves)
            ]
        ]
