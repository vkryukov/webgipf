module GipfTests exposing (..)

import Expect
import Gipf
    exposing
        ( BoardPieces
        , Color(..)
        , Coord
        , Kind(..)
        , Move
        , Piece
        , allMoves
        , availableMoves
        , boardPointQ
        , boardPoints
        , boardToString
        , connectedGroupsOfFour
        , coordinatesSlice
        , edgeBoardPointQ
        , interiorBoardPointQ
        , nameToCoord
        , neighbors
        , performMove
        , piecesToBoard
        , standardStartingBoard
        , stringToBoard
        , stringToBoardWithDefault
        )
import List exposing (sortWith)
import Test exposing (..)
import Tools exposing (extendSublistWithJustItems, largestPrefixWithoutNothing)


sortedEqual : List Coord -> List Coord -> Expect.Expectation
sortedEqual a b =
    Expect.equal (List.sort a) (List.sort b)


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
                    Expect.equal True (boardPointQ ( 4, 4 ))
            , test "checks a point outside the board" <|
                \_ ->
                    Expect.equal False (boardPointQ ( 9, 9 ))
            ]
        , describe "interiorBoardPointQ function"
            [ test "checks an interior point" <|
                \_ ->
                    Expect.equal True (interiorBoardPointQ ( 4, 4 ))
            , test "checks a non-interior point" <|
                \_ ->
                    Expect.equal False (interiorBoardPointQ ( 0, 0 ))
            ]
        , describe "edgeBoardPointQ function"
            [ test "checks an edge point" <|
                \_ ->
                    Expect.equal True (edgeBoardPointQ ( 0, 4 ))
            , test "checks a non-edge point" <|
                \_ ->
                    Expect.equal False (edgeBoardPointQ ( 4, 4 ))
            ]
        , describe "neighbors function"
            [ neighborsTest "finds neighbors of a point at (4, 4)"
                ( 4, 4 )
                [ ( 4, 5 )
                , ( 4, 3 )
                , ( 5, 4 )
                , ( 3, 4 )
                , ( 5, 5 )
                , ( 3, 3 )
                ]
            , neighborsTest "finds neighbors of a point at (0, 0)"
                ( 0, 0 )
                [ ( 1, 1 )
                ]
            , neighborsTest "finds neighbors of a point at (0, 4)"
                ( 0, 4 )
                [ ( 1, 4 )
                ]
            , neighborsTest "finds neighbors of a point at (1, 0)"
                ( 1, 0 )
                [ ( 1, 1 )
                , ( 2, 1 )
                ]
            , neighborsTest "finds neighbors of a point at (7, 7)"
                ( 7, 7 )
                [ ( 6, 7 )
                , ( 7, 6 )
                , ( 6, 6 )
                ]
            ]
        , describe "boardPoints function"
            [ test "finds all board points" <|
                \_ ->
                    Expect.equal 61 (List.length boardPoints)
            ]
        , describe "nameToCoord Tests"
            [ test "Converts 'a1' to ( 0,  0 )" <|
                \_ ->
                    Expect.equal (Just ( 0, 0 )) (nameToCoord "a1")
            , test "Converts 'a5' to ( 0,  4 )" <|
                \_ ->
                    Expect.equal (Just ( 0, 4 )) (nameToCoord "a5")
            , test "Converts 'e1' to ( 4,  0 )" <|
                \_ ->
                    Expect.equal (Just ( 4, 0 )) (nameToCoord "e1")
            , test "Converts 'e9' to ( 4,  8 )" <|
                \_ ->
                    Expect.equal (Just ( 4, 8 )) (nameToCoord "e9")
            , test "Converts 'g6' to ( 6,  7 )" <|
                \_ ->
                    Expect.equal (Just ( 6, 7 )) (nameToCoord "g6")
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
            , test "converts random board 2 to string" <|
                \_ -> Expect.equal "GKb2 GKd3 GKd4 GKd7 GKe7 GKe8 GKf3 GKf4 GKh2 GWb4 GWb5 GWc2 GWc6 GWd2 GWd5 GWe6 GWf7 GWg4 GWh3 GWh4 Kc3 Ke2 Kf6 Kg5 Kg6 Wb3 Wd6 We3 We5 Wf2 Wf5 Wg3" (boardToString randomBoard2)
            , test "converts random board 2 to string and back" <|
                \_ ->
                    let
                        b =
                            stringToBoard (boardToString randomBoard2)
                    in
                    case b of
                        Just b2 ->
                            Expect.equal (boardToString randomBoard2) (boardToString b2)

                        Nothing ->
                            Expect.fail "stringToBoard returned Nothing"
            ]
        ]


compareMoves : Move -> Move -> Order
compareMoves move1 move2 =
    case compare move1.from move2.from of
        EQ ->
            compare move1.to move2.to

        order ->
            order


sortMoves : List Move -> List Move
sortMoves moves =
    sortWith compareMoves moves


randomBoard1 : BoardPieces
randomBoard1 =
    piecesToBoard
        [ Piece ( 2, 2 ) White Regular
        , Piece ( 4, 1 ) White Gipf
        , Piece ( 1, 4 ) White Regular
        , Piece ( 2, 3 ) White Gipf
        , Piece ( 1, 2 ) White Regular
        , Piece ( 7, 4 ) White Gipf
        , Piece ( 6, 7 ) Black Regular
        , Piece ( 2, 4 ) White Regular
        , Piece ( 5, 7 ) White Gipf
        , Piece ( 4, 3 ) White Gipf
        , Piece ( 3, 1 ) White Gipf
        , Piece ( 3, 4 ) White Regular
        , Piece ( 4, 4 ) Black Regular
        , Piece ( 4, 5 ) White Gipf
        , Piece ( 7, 5 ) White Gipf
        , Piece ( 4, 6 ) White Gipf
        , Piece ( 7, 7 ) Black Regular
        , Piece ( 2, 5 ) White Gipf
        , Piece ( 5, 6 ) White Regular
        , Piece ( 5, 2 ) White Gipf
        , Piece ( 3, 3 ) White Regular
        , Piece ( 5, 4 ) White Gipf
        , Piece ( 7, 6 ) White Gipf
        , Piece ( 2, 1 ) Black Regular
        , Piece ( 6, 5 ) White Gipf
        , Piece ( 1, 3 ) White Regular
        , Piece ( 3, 6 ) Black Regular
        , Piece ( 6, 3 ) White Regular
        , Piece ( 3, 2 ) White Gipf
        , Piece ( 1, 1 ) White Gipf
        , Piece ( 6, 6 ) Black Regular
        , Piece ( 4, 2 ) White Gipf
        ]


randomBoard1Moves : List Move
randomBoard1Moves =
    [ { from = ( 0, 0 ), to = ( 1, 1 ) }
    , { from = ( 0, 2 ), to = ( 1, 3 ) }
    , { from = ( 0, 3 ), to = ( 1, 3 ) }
    , { from = ( 0, 3 ), to = ( 1, 4 ) }
    , { from = ( 0, 4 ), to = ( 1, 4 ) }
    , { from = ( 1, 5 ), to = ( 2, 5 ) }
    , { from = ( 2, 0 ), to = ( 3, 1 ) }
    , { from = ( 3, 0 ), to = ( 3, 1 ) }
    , { from = ( 3, 7 ), to = ( 3, 6 ) }
    , { from = ( 3, 7 ), to = ( 4, 7 ) }
    , { from = ( 4, 0 ), to = ( 4, 1 ) }
    , { from = ( 4, 8 ), to = ( 4, 7 ) }
    , { from = ( 5, 1 ), to = ( 5, 2 ) }
    , { from = ( 5, 8 ), to = ( 5, 7 ) }
    , { from = ( 5, 8 ), to = ( 4, 7 ) }
    , { from = ( 6, 2 ), to = ( 6, 3 ) }
    , { from = ( 6, 8 ), to = ( 6, 7 ) }
    , { from = ( 6, 8 ), to = ( 5, 7 ) }
    , { from = ( 7, 3 ), to = ( 6, 3 ) }
    , { from = ( 8, 4 ), to = ( 7, 4 ) }
    , { from = ( 8, 5 ), to = ( 7, 5 ) }
    , { from = ( 8, 6 ), to = ( 7, 5 ) }
    , { from = ( 8, 7 ), to = ( 7, 7 ) }
    , { from = ( 8, 8 ), to = ( 7, 7 ) }
    ]


randomBoard2 : BoardPieces
randomBoard2 =
    piecesToBoard
        [ Piece ( 4, 7 ) Black Gipf
        , Piece ( 4, 4 ) White Regular
        , Piece ( 2, 5 ) White Gipf
        , Piece ( 1, 3 ) White Gipf
        , Piece ( 2, 2 ) Black Regular
        , Piece ( 7, 5 ) White Gipf
        , Piece ( 5, 4 ) Black Gipf
        , Piece ( 4, 6 ) Black Gipf
        , Piece ( 5, 7 ) White Gipf
        , Piece ( 5, 2 ) White Regular
        , Piece ( 4, 5 ) White Gipf
        , Piece ( 5, 3 ) Black Gipf
        , Piece ( 3, 6 ) Black Gipf
        , Piece ( 3, 2 ) Black Gipf
        , Piece ( 7, 6 ) White Gipf
        , Piece ( 3, 4 ) White Gipf
        , Piece ( 3, 3 ) Black Gipf
        , Piece ( 3, 5 ) White Regular
        , Piece ( 4, 2 ) White Regular
        , Piece ( 6, 7 ) Black Regular
        , Piece ( 3, 1 ) White Gipf
        , Piece ( 1, 1 ) Black Gipf
        , Piece ( 6, 4 ) White Regular
        , Piece ( 6, 5 ) White Gipf
        , Piece ( 5, 5 ) White Regular
        , Piece ( 5, 6 ) Black Regular
        , Piece ( 7, 4 ) Black Gipf
        , Piece ( 2, 1 ) White Gipf
        , Piece ( 4, 1 ) Black Regular
        , Piece ( 1, 2 ) White Regular
        , Piece ( 1, 4 ) White Gipf
        , Piece ( 6, 6 ) Black Regular
        ]


randomBoard2Moves : List Move
randomBoard2Moves =
    [ { from = ( 0, 0 ), to = ( 1, 1 ) }
    , { from = ( 0, 1 ), to = ( 1, 2 ) }
    , { from = ( 0, 2 ), to = ( 1, 3 ) }
    , { from = ( 0, 3 ), to = ( 1, 3 ) }
    , { from = ( 0, 4 ), to = ( 1, 4 ) }
    , { from = ( 1, 0 ), to = ( 2, 1 ) }
    , { from = ( 2, 0 ), to = ( 2, 1 ) }
    , { from = ( 2, 6 ), to = ( 2, 5 ) }
    , { from = ( 3, 0 ), to = ( 4, 1 ) }
    , { from = ( 3, 7 ), to = ( 4, 7 ) }
    , { from = ( 4, 0 ), to = ( 4, 1 ) }
    , { from = ( 4, 8 ), to = ( 4, 7 ) }
    , { from = ( 6, 2 ), to = ( 6, 3 ) }
    , { from = ( 6, 8 ), to = ( 6, 7 ) }
    , { from = ( 6, 8 ), to = ( 5, 7 ) }
    , { from = ( 7, 3 ), to = ( 7, 4 ) }
    , { from = ( 7, 3 ), to = ( 6, 3 ) }
    , { from = ( 7, 8 ), to = ( 7, 7 ) }
    , { from = ( 7, 8 ), to = ( 6, 7 ) }
    , { from = ( 8, 4 ), to = ( 7, 4 ) }
    , { from = ( 8, 5 ), to = ( 7, 4 ) }
    , { from = ( 8, 7 ), to = ( 7, 7 ) }
    , { from = ( 8, 7 ), to = ( 7, 6 ) }
    , { from = ( 8, 8 ), to = ( 7, 7 ) }
    ]


testPerformMove : String -> String -> Move -> Color -> Kind -> String -> Test
testPerformMove description startingBoardString move color kind expectedBoardString =
    test description <|
        \_ ->
            let
                b =
                    stringToBoard startingBoardString
            in
            case b of
                Just b2 ->
                    case performMove move color kind b2 of
                        Just b3 ->
                            Expect.equal expectedBoardString (boardToString b3)

                        Nothing ->
                            Expect.fail "performMove returned Nothing"

                Nothing ->
                    Expect.fail "stringToBoard returned Nothing"


movesTest : Test
movesTest =
    describe "Move tests"
        [ describe "boardSlice function"
            [ boardSliceTest "checks board slice from {2,0} to {6,4}"
                ( 2, 0 )
                ( 6, 4 )
                [ ( 3, 1 )
                , ( 4, 2 )
                , ( 5, 3 )
                , ( 6, 4 )
                , ( 7, 5 )
                ]
            , boardSliceTest "checks board slice from {4,8} to {4,5}"
                ( 4, 8 )
                ( 4, 5 )
                [ ( 4, 1 )
                , ( 4, 2 )
                , ( 4, 3 )
                , ( 4, 4 )
                , ( 4, 5 )
                , ( 4, 6 )
                , ( 4, 7 )
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
        , describe "performing moves"
            [ testPerformMove "making one move"
                "GKb5 GKe2 GKh5 GWb2 GWe8 GWh2"
                { from = ( 0, 0 ), to = ( 1, 1 ) }
                White
                Regular
                "GKb5 GKe2 GKh5 GWc3 GWe8 GWh2 Wb2"
            ]
        ]


connectedPiecestest : Test
connectedPiecestest =
    describe "Connected pieces"
        [ test "adjacent pieces bug" <|
            \_ ->
                Expect.equal
                    (connectedGroupsOfFour
                        (stringToBoardWithDefault "GKb5 GKe2 GKh5 GWb2 GWe5 GWe8 Kc4 Kd4 Ke4 Kf3 Kh3 We3 Wf4 Wg2 Wg3 Wg4 Wh2")
                    )
                    []
        ]


toolsTest : Test
toolsTest =
    describe "Tools tests"
        [ describe "sortCoords function"
            [ test "sorts a list of coordinates" <|
                \_ ->
                    sortedEqual [ ( 1, 1 ), ( 2, 2 ), ( 3, 3 ) ] [ ( 3, 3 ), ( 1, 1 ), ( 2, 2 ) ]
            ]
        , describe "largestPrefixWithoutNothihg function"
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
