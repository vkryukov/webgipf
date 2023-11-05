module GipfTests exposing (..)

import Expect
import Gipf
import Test exposing (..)


sortByXY : List Gipf.Coord -> List Gipf.Coord
sortByXY coords =
    coords
        |> List.sortBy .y
        |> List.sortBy .x


sortedEqual : List Gipf.Coord -> List Gipf.Coord -> Expect.Expectation
sortedEqual a b =
    Expect.equal (sortByXY a) (sortByXY b)


neighborsTest : String -> Gipf.Coord -> List Gipf.Coord -> Test
neighborsTest description point expectedNeighbors =
    test description <|
        \_ ->
            sortedEqual expectedNeighbors (Gipf.neighbors point)


boardSliceTest : String -> Gipf.Coord -> Gipf.Coord -> List Gipf.Coord -> Test
boardSliceTest description start end expectedSlice =
    test description <|
        \_ ->
            sortedEqual expectedSlice (Gipf.boardSlice start end)


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
                    Expect.equal 61 (List.length Gipf.boardPoints)
            ]
        , describe "boardSlice function"
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
        ]
