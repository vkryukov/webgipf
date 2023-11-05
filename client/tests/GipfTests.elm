module GipfTests exposing (..)

import Dict
import Expect
import Gipf exposing (BoardPieces, Coord, Kind(..), Move, allMoves, availableMoves, boardPointQ, boardPoints, boardToString, coordinatesSlice, edgeBoardPointQ, interiorBoardPointQ, nameToCoord, neighbors, standardStartingBoard)
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
            sortedEqual expectedSlice (coordinatesSlice start end)


board : Test
board =
    describe "Board tests"
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
        , describe "nameToCoord Tests"
            [ test "Converts 'a1' to { x = 0, y = 0 }" <|
                \_ ->
                    Expect.equal (Just { x = 0, y = 0 }) (nameToCoord "a1")
            , test "Converts 'a5' to { x = 0, y = 4 }" <|
                \_ ->
                    Expect.equal (Just { x = 0, y = 4 }) (nameToCoord "a5")
            , test "Converts 'e1' to { x = 4, y = 0 }" <|
                \_ ->
                    Expect.equal (Just { x = 4, y = 0 }) (nameToCoord "e1")
            , test "Converts 'e9' to { x = 4, y = 8 }" <|
                \_ ->
                    Expect.equal (Just { x = 4, y = 8 }) (nameToCoord "e9")
            , test "Converts 'g6' to { x = 6, y = 7 }" <|
                \_ ->
                    Expect.equal (Just { x = 6, y = 7 }) (nameToCoord "g6")
            , test "Invalid 'a0' returns Nothing" <|
                \_ ->
                    Expect.equal Nothing (nameToCoord "a0")
            , test "Invalid 'a6' returns Nothing" <|
                \_ ->
                    Expect.equal Nothing (nameToCoord "a6")
            , test "Invalid 'gf' returns Nothing" <|
                \_ ->
                    Expect.equal Nothing (nameToCoord "gf")
            ]
        , describe "boardToString function"
            [ test "converts standard board to string" <|
                \_ ->
                    Expect.equal "GKb5 GKe2 GKh5 GWb2 GWe8 GWh2" (boardToString standardStartingBoard)
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


randomBoard1 : BoardPieces
randomBoard1 =
    Dict.fromList
        [ ( ( 2, 2 ), White )
        , ( ( 4, 1 ), WhiteGipf )
        , ( ( 1, 4 ), White )
        , ( ( 2, 3 ), BlackGipf )
        , ( ( 1, 2 ), White )
        , ( ( 7, 4 ), BlackGipf )
        , ( ( 6, 7 ), Black )
        , ( ( 2, 4 ), White )
        , ( ( 5, 7 ), BlackGipf )
        , ( ( 4, 3 ), BlackGipf )
        , ( ( 3, 1 ), WhiteGipf )
        , ( ( 3, 4 ), White )
        , ( ( 4, 4 ), Black )
        , ( ( 4, 5 ), WhiteGipf )
        , ( ( 7, 5 ), BlackGipf )
        , ( ( 4, 6 ), BlackGipf )
        , ( ( 7, 7 ), Black )
        , ( ( 2, 5 ), WhiteGipf )
        , ( ( 5, 6 ), White )
        , ( ( 5, 2 ), BlackGipf )
        , ( ( 3, 3 ), White )
        , ( ( 5, 4 ), BlackGipf )
        , ( ( 7, 6 ), WhiteGipf )
        , ( ( 2, 1 ), Black )
        , ( ( 6, 5 ), WhiteGipf )
        , ( ( 1, 3 ), White )
        , ( ( 3, 6 ), Black )
        , ( ( 6, 3 ), White )
        , ( ( 3, 2 ), WhiteGipf )
        , ( ( 1, 1 ), WhiteGipf )
        , ( ( 6, 6 ), Black )
        , ( ( 4, 2 ), BlackGipf )
        ]


randomBoard1Moves : List Move
randomBoard1Moves =
    [ { from = { x = 0, y = 0 }, to = { x = 1, y = 1 } }
    , { from = { x = 0, y = 2 }, to = { x = 1, y = 3 } }
    , { from = { x = 0, y = 3 }, to = { x = 1, y = 3 } }
    , { from = { x = 0, y = 3 }, to = { x = 1, y = 4 } }
    , { from = { x = 0, y = 4 }, to = { x = 1, y = 4 } }
    , { from = { x = 1, y = 5 }, to = { x = 2, y = 5 } }
    , { from = { x = 2, y = 0 }, to = { x = 3, y = 1 } }
    , { from = { x = 3, y = 0 }, to = { x = 3, y = 1 } }
    , { from = { x = 3, y = 7 }, to = { x = 3, y = 6 } }
    , { from = { x = 3, y = 7 }, to = { x = 4, y = 7 } }
    , { from = { x = 4, y = 0 }, to = { x = 4, y = 1 } }
    , { from = { x = 4, y = 8 }, to = { x = 4, y = 7 } }
    , { from = { x = 5, y = 1 }, to = { x = 5, y = 2 } }
    , { from = { x = 5, y = 8 }, to = { x = 5, y = 7 } }
    , { from = { x = 5, y = 8 }, to = { x = 4, y = 7 } }
    , { from = { x = 6, y = 2 }, to = { x = 6, y = 3 } }
    , { from = { x = 6, y = 8 }, to = { x = 6, y = 7 } }
    , { from = { x = 6, y = 8 }, to = { x = 5, y = 7 } }
    , { from = { x = 7, y = 3 }, to = { x = 6, y = 3 } }
    , { from = { x = 8, y = 4 }, to = { x = 7, y = 4 } }
    , { from = { x = 8, y = 5 }, to = { x = 7, y = 5 } }
    , { from = { x = 8, y = 6 }, to = { x = 7, y = 5 } }
    , { from = { x = 8, y = 7 }, to = { x = 7, y = 7 } }
    , { from = { x = 8, y = 8 }, to = { x = 7, y = 7 } }
    ]


randomBoard2 : BoardPieces
randomBoard2 =
    Dict.fromList
        [ ( ( 4, 7 ), BlackGipf )
        , ( ( 4, 4 ), White )
        , ( ( 2, 5 ), WhiteGipf )
        , ( ( 1, 3 ), WhiteGipf )
        , ( ( 2, 2 ), Black )
        , ( ( 7, 5 ), WhiteGipf )
        , ( ( 5, 4 ), BlackGipf )
        , ( ( 4, 6 ), BlackGipf )
        , ( ( 5, 7 ), WhiteGipf )
        , ( ( 5, 2 ), White )
        , ( ( 4, 5 ), WhiteGipf )
        , ( ( 5, 3 ), BlackGipf )
        , ( ( 3, 6 ), BlackGipf )
        , ( ( 3, 2 ), BlackGipf )
        , ( ( 7, 6 ), WhiteGipf )
        , ( ( 3, 4 ), WhiteGipf )
        , ( ( 3, 3 ), BlackGipf )
        , ( ( 3, 5 ), White )
        , ( ( 4, 2 ), White )
        , ( ( 6, 7 ), Black )
        , ( ( 3, 1 ), WhiteGipf )
        , ( ( 1, 1 ), BlackGipf )
        , ( ( 6, 4 ), White )
        , ( ( 6, 5 ), WhiteGipf )
        , ( ( 5, 5 ), White )
        , ( ( 5, 6 ), Black )
        , ( ( 7, 4 ), BlackGipf )
        , ( ( 2, 1 ), WhiteGipf )
        , ( ( 4, 1 ), Black )
        , ( ( 1, 2 ), White )
        , ( ( 1, 4 ), WhiteGipf )
        , ( ( 6, 6 ), Black )
        ]


randomBoard2Moves : List Move
randomBoard2Moves =
    [ { from = { x = 0, y = 0 }, to = { x = 1, y = 1 } }
    , { from = { x = 0, y = 1 }, to = { x = 1, y = 2 } }
    , { from = { x = 0, y = 2 }, to = { x = 1, y = 3 } }
    , { from = { x = 0, y = 3 }, to = { x = 1, y = 3 } }
    , { from = { x = 0, y = 4 }, to = { x = 1, y = 4 } }
    , { from = { x = 1, y = 0 }, to = { x = 2, y = 1 } }
    , { from = { x = 2, y = 0 }, to = { x = 2, y = 1 } }
    , { from = { x = 2, y = 6 }, to = { x = 2, y = 5 } }
    , { from = { x = 3, y = 0 }, to = { x = 4, y = 1 } }
    , { from = { x = 3, y = 7 }, to = { x = 4, y = 7 } }
    , { from = { x = 4, y = 0 }, to = { x = 4, y = 1 } }
    , { from = { x = 4, y = 8 }, to = { x = 4, y = 7 } }
    , { from = { x = 6, y = 2 }, to = { x = 6, y = 3 } }
    , { from = { x = 6, y = 8 }, to = { x = 6, y = 7 } }
    , { from = { x = 6, y = 8 }, to = { x = 5, y = 7 } }
    , { from = { x = 7, y = 3 }, to = { x = 7, y = 4 } }
    , { from = { x = 7, y = 3 }, to = { x = 6, y = 3 } }
    , { from = { x = 7, y = 8 }, to = { x = 7, y = 7 } }
    , { from = { x = 7, y = 8 }, to = { x = 6, y = 7 } }
    , { from = { x = 8, y = 4 }, to = { x = 7, y = 4 } }
    , { from = { x = 8, y = 5 }, to = { x = 7, y = 4 } }
    , { from = { x = 8, y = 7 }, to = { x = 7, y = 7 } }
    , { from = { x = 8, y = 7 }, to = { x = 7, y = 6 } }
    , { from = { x = 8, y = 8 }, to = { x = 7, y = 7 } }
    ]


movesTest : Test
movesTest =
    describe "Move tests"
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
            , test "available moves on random board 1" <|
                \_ ->
                    Expect.equal (sortMoves randomBoard1Moves) (sortMoves (availableMoves randomBoard1))
            , test "available moves on random board 2" <|
                \_ ->
                    Expect.equal (sortMoves randomBoard2Moves) (sortMoves (availableMoves randomBoard2))
            ]
        ]


toolsTest : Test
toolsTest =
    describe "Tools tests"
        [ describe "sortCoords function"
            [ test "sorts a list of coordinates" <|
                \_ ->
                    sortedEqual [ { x = 1, y = 1 }, { x = 2, y = 2 }, { x = 3, y = 3 } ] [ { x = 3, y = 3 }, { x = 1, y = 1 }, { x = 2, y = 2 } ]
            ]
        , describe "largestPrefixWithoutNothihg function"
            [ test "finds the largest prefix without Nothing" <|
                \_ ->
                    Expect.equal [ 1, 2, 3 ] (Gipf.largestPrefixWithoutNothing [ Just 1, Just 2, Just 3, Nothing, Just 4, Just 5 ])
            , test "finds the largest prefix without Nothing when it's emmpty" <|
                \_ ->
                    Expect.equal [] (Gipf.largestPrefixWithoutNothing [ Nothing, Just 1, Just 2, Just 3, Nothing, Just 4, Just 5 ])
            ]
        ]
