module GipfTests exposing (..)

import DebugTools exposing (..)
import Dict
import Expect
import Gipf exposing (..)
import List exposing (sortWith)
import Test exposing (..)


sortedEqual : List Coord -> List Coord -> Expect.Expectation
sortedEqual a b =
    Expect.equal (List.sort a) (List.sort b)


sortedEqualTest : Test
sortedEqualTest =
    describe "sortCoords function"
        [ test "sorts a list of coordinates" <|
            \_ ->
                sortedEqual [ ( 1, 1 ), ( 2, 2 ), ( 3, 3 ) ] [ ( 3, 3 ), ( 1, 1 ), ( 2, 2 ) ]
        ]


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


compareDirections : Direction -> Direction -> Order
compareDirections dir1 dir2 =
    case compare dir1.from dir2.from of
        EQ ->
            compare dir1.to dir2.to

        order ->
            order


sortDirections : List Direction -> List Direction
sortDirections dirs =
    sortWith compareDirections dirs


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


randomBoard1Moves : List Direction
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


randomBoard2Moves : List Direction
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


testInsertPieceWithMove : String -> String -> Direction -> Color -> Kind -> String -> Test
testInsertPieceWithMove description startingBoardString move color kind expectedBoardString =
    test description <|
        \_ ->
            let
                b =
                    stringToBoard startingBoardString
            in
            case b of
                Just b2 ->
                    case insertPieceWithMove move color kind b2 of
                        Just b3 ->
                            Expect.equal expectedBoardString (boardToString b3)

                        Nothing ->
                            Expect.fail "performMove returned Nothing"

                Nothing ->
                    Expect.fail "stringToBoard returned Nothing"


availableMovesTest : Test
availableMovesTest =
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
                    Expect.equal (sortDirections randomBoard1Moves) (sortDirections (availableMoves randomBoard1))
            , test "available moves on random board 2" <|
                \_ ->
                    Expect.equal (sortDirections randomBoard2Moves) (sortDirections (availableMoves randomBoard2))
            ]
        , describe "insert piece with move"
            [ testInsertPieceWithMove "making one move"
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
        [ describe "connectedGroupOfFour func"
            [ test "empty list" <|
                \_ ->
                    Expect.equal
                        (connectedGroupOfFour [])
                        Nothing
            , test "short list" <|
                \_ ->
                    Expect.equal
                        (connectedGroupOfFour
                            [ Just (Piece ( 1, 1 ) White Regular)
                            , Just (Piece ( 1, 2 ) White Regular)
                            , Just (Piece ( 1, 3 ) White Regular)
                            ]
                        )
                        Nothing
            , test "four same color" <|
                \_ ->
                    Expect.equal
                        (connectedGroupOfFour
                            [ Nothing
                            , Just (Piece ( 1, 1 ) White Regular)
                            , Just (Piece ( 1, 2 ) White Regular)
                            , Just (Piece ( 1, 3 ) White Regular)
                            , Just (Piece ( 1, 4 ) White Gipf)
                            , Nothing
                            ]
                        )
                        (Just
                            [ Piece ( 1, 1 ) White Regular
                            , Piece ( 1, 2 ) White Regular
                            , Piece ( 1, 3 ) White Regular
                            , Piece ( 1, 4 ) White Gipf
                            ]
                        )
            , test "four same color with adjacent pieces" <|
                \_ ->
                    Expect.equal
                        (connectedGroupOfFour
                            [ Nothing
                            , Just (Piece ( 1, 1 ) White Regular)
                            , Just (Piece ( 1, 2 ) White Regular)
                            , Just (Piece ( 1, 3 ) White Regular)
                            , Just (Piece ( 1, 4 ) White Gipf)
                            , Just (Piece ( 1, 5 ) Black Gipf)
                            , Nothing
                            ]
                        )
                        (Just
                            [ Piece ( 1, 1 ) White Regular
                            , Piece ( 1, 2 ) White Regular
                            , Piece ( 1, 3 ) White Regular
                            , Piece ( 1, 4 ) White Gipf
                            , Piece ( 1, 5 ) Black Gipf
                            ]
                        )
            ]
        , test "adjacent pieces bug" <|
            \_ ->
                Expect.equal
                    (connectedGroupsOfFour
                        (stringToBoardWithDefault "GKb5 GKe2 GKh5 GWb2 GWe5 GWe8 Kc4 Kd4 Ke4 Kf3 Kh3 We3 Wf4 Wg2 Wg3 Wg4 Wh2")
                    )
                    [ [ { color = Black, coord = ( 2, 3 ), kind = Regular }
                      , { color = Black, coord = ( 3, 3 ), kind = Regular }
                      , { color = Black, coord = ( 4, 3 ), kind = Regular }
                      , { color = Black, coord = ( 5, 3 ), kind = Regular }
                      , { color = White, coord = ( 6, 3 ), kind = Regular }
                      ]
                    , [ { color = White, coord = ( 4, 4 ), kind = Gipf }
                      , { color = White, coord = ( 5, 4 ), kind = Regular }
                      , { color = White, coord = ( 6, 4 ), kind = Regular }
                      , { color = White, coord = ( 7, 4 ), kind = Regular }
                      ]
                    ]
        ]


stringToMoveTest : Test
stringToMoveTest =
    describe "stringToMove function"
        [ test "a1-b2" <|
            \_ ->
                Expect.equal
                    (Just { direction = { from = ( 0, 0 ), to = ( 1, 1 ) }, color = White, kind = Regular })
                    (stringToMove "a1-b2")
        , test "Wa1-b2" <|
            \_ ->
                Expect.equal
                    (Just { direction = { from = ( 0, 0 ), to = ( 1, 1 ) }, color = White, kind = Regular })
                    (stringToMove "Wa1-b2")
        , test "Ka1-b2" <|
            \_ ->
                Expect.equal
                    (Just { direction = { from = ( 0, 0 ), to = ( 1, 1 ) }, color = Black, kind = Regular })
                    (stringToMove "Ka1-b2")
        , test "Ga1-b2" <|
            \_ ->
                Expect.equal
                    (Just { direction = { from = ( 0, 0 ), to = ( 1, 1 ) }, color = White, kind = Gipf })
                    (stringToMove "Ga1-b2")
        , test "GWa1-b2" <|
            \_ ->
                Expect.equal
                    (Just { direction = { from = ( 0, 0 ), to = ( 1, 1 ) }, color = White, kind = Gipf })
                    (stringToMove "GWa1-b2")
        , test "GKa1-b2" <|
            \_ ->
                Expect.equal
                    (Just { direction = { from = ( 0, 0 ), to = ( 1, 1 ) }, color = Black, kind = Gipf })
                    (stringToMove "GKa1-b2")
        , test "invalid format" <|
            \_ ->
                Expect.equal
                    Nothing
                    (stringToMove "a1b2")
        , test "invalid coordinates" <| \_ -> Expect.equal Nothing (stringToMove "a1-b7")
        ]


performMoveWithDefaultColorTest : Test
performMoveWithDefaultColorTest =
    describe "performMoveWithDefaultColor tests"
        [ test "Perform move on an empty board" <|
            \_ ->
                let
                    direction =
                        { from = ( 4, 0 ), to = ( 4, 1 ) }

                    kind =
                        Gipf

                    game =
                        emptyGame
                in
                Expect.equal (performMoveWithDefaultColor direction kind game)
                    (Just
                        { actionHistory = [ MoveAction { color = White, direction = { from = ( 4, 0 ), to = ( 4, 1 ) }, kind = Gipf } ]
                        , blackCount = { captured = 0, own = 18 }
                        , blackGipfCount = 0
                        , blackPlayedNonGipf = False
                        , board = Dict.fromList [ ( ( 4, 1 ), { color = White, coord = ( 4, 1 ), kind = Gipf } ) ]
                        , currentColor = Black
                        , currentKind = Gipf
                        , currentPlayerFourStones = []
                        , isBasicGame = False
                        , otherPlayerFourStones = []
                        , state = WaitingForMove
                        , whiteCount = { captured = 0, own = 16 }
                        , whiteGipfCount = 1
                        , whitePlayedNonGipf = False
                        }
                    )
        ]


game1 : Maybe Game
game1 =
    -- http://www.gipf.com/gipf/archives/game010821_01.html
    stringToGame "GWi3-h3 GKb6-c6 GWi2-h3 GKc7-c6 GWi2-h3 GKc7-c6 Wi4-h4 Ka5-b5 Wi3-h4 Ki4-h4 Wi3-h4 Kg7-g6 Wb6-c6 Kb6-c6 Wi3-h4 Kf8-f7 Wg1-f2 Ka5-b5"


stringToGameTest : Test
stringToGameTest =
    describe "stringToGame tests"
        [ test "Playing Gipf pieces should change Gipf count and pieces count" <|
            \_ ->
                Expect.equal (stringToGame "GWf1-e2 GKe1-e2 GWf1-e2")
                    (Just
                        { actionHistory = [ MoveAction { color = White, direction = { from = ( 5, 1 ), to = ( 4, 1 ) }, kind = Gipf }, MoveAction { color = Black, direction = { from = ( 4, 0 ), to = ( 4, 1 ) }, kind = Gipf }, MoveAction { color = White, direction = { from = ( 5, 1 ), to = ( 4, 1 ) }, kind = Gipf } ]
                        , blackCount = { captured = 0, own = 16 }
                        , blackGipfCount = 1
                        , blackPlayedNonGipf = False
                        , board = Dict.fromList [ ( ( 3, 1 ), { color = Black, coord = ( 3, 1 ), kind = Gipf } ), ( ( 4, 1 ), { color = White, coord = ( 4, 1 ), kind = Gipf } ), ( ( 4, 2 ), { color = White, coord = ( 4, 2 ), kind = Gipf } ) ]
                        , currentColor = Black
                        , currentKind = Gipf
                        , currentPlayerFourStones = []
                        , isBasicGame = False
                        , otherPlayerFourStones = []
                        , state = WaitingForMove
                        , whiteCount = { captured = 0, own = 14 }
                        , whiteGipfCount = 2
                        , whitePlayedNonGipf = False
                        }
                    )
        , test "Playing standard game moves should result in a good game" <|
            \_ ->
                Expect.equal (stringToGame "GWe1-e2 GKa1-b2 GWa5-b5 GKe9-e8 GWi5-h5 GKi1-h2")
                    (Just
                        { actionHistory = [ MoveAction { color = Black, direction = { from = ( 8, 4 ), to = ( 7, 4 ) }, kind = Gipf }, MoveAction { color = White, direction = { from = ( 8, 8 ), to = ( 7, 7 ) }, kind = Gipf }, MoveAction { color = Black, direction = { from = ( 4, 8 ), to = ( 4, 7 ) }, kind = Gipf }, MoveAction { color = White, direction = { from = ( 0, 4 ), to = ( 1, 4 ) }, kind = Gipf }, MoveAction { color = Black, direction = { from = ( 0, 0 ), to = ( 1, 1 ) }, kind = Gipf }, MoveAction { color = White, direction = { from = ( 4, 0 ), to = ( 4, 1 ) }, kind = Gipf } ]
                        , blackCount = { captured = 0, own = 12 }
                        , blackGipfCount = 3
                        , blackPlayedNonGipf = False
                        , board = Dict.fromList [ ( ( 1, 1 ), { color = Black, coord = ( 1, 1 ), kind = Gipf } ), ( ( 1, 4 ), { color = White, coord = ( 1, 4 ), kind = Gipf } ), ( ( 4, 1 ), { color = White, coord = ( 4, 1 ), kind = Gipf } ), ( ( 4, 7 ), { color = Black, coord = ( 4, 7 ), kind = Gipf } ), ( ( 7, 4 ), { color = Black, coord = ( 7, 4 ), kind = Gipf } ), ( ( 7, 7 ), { color = White, coord = ( 7, 7 ), kind = Gipf } ) ]
                        , currentColor = White
                        , currentKind = Gipf
                        , currentPlayerFourStones = []
                        , isBasicGame = False
                        , otherPlayerFourStones = []
                        , state = WaitingForMove
                        , whiteCount = { captured = 0, own = 12 }
                        , whiteGipfCount = 3
                        , whitePlayedNonGipf = False
                        }
                    )
        , test "Standard game" <|
            \_ ->
                Expect.equal standardGame
                    { actionHistory = [ MoveAction { color = Black, direction = { from = ( 8, 4 ), to = ( 7, 4 ) }, kind = Gipf }, MoveAction { color = White, direction = { from = ( 8, 8 ), to = ( 7, 7 ) }, kind = Gipf }, MoveAction { color = Black, direction = { from = ( 4, 8 ), to = ( 4, 7 ) }, kind = Gipf }, MoveAction { color = White, direction = { from = ( 0, 4 ), to = ( 1, 4 ) }, kind = Gipf }, MoveAction { color = Black, direction = { from = ( 0, 0 ), to = ( 1, 1 ) }, kind = Gipf }, MoveAction { color = White, direction = { from = ( 4, 0 ), to = ( 4, 1 ) }, kind = Gipf } ]
                    , blackCount = { captured = 0, own = 12 }
                    , blackGipfCount = 3
                    , blackPlayedNonGipf = True
                    , board = Dict.fromList [ ( ( 1, 1 ), { color = Black, coord = ( 1, 1 ), kind = Gipf } ), ( ( 1, 4 ), { color = White, coord = ( 1, 4 ), kind = Gipf } ), ( ( 4, 1 ), { color = White, coord = ( 4, 1 ), kind = Gipf } ), ( ( 4, 7 ), { color = Black, coord = ( 4, 7 ), kind = Gipf } ), ( ( 7, 4 ), { color = Black, coord = ( 7, 4 ), kind = Gipf } ), ( ( 7, 7 ), { color = White, coord = ( 7, 7 ), kind = Gipf } ) ]
                    , currentColor = White
                    , currentKind = Regular
                    , currentPlayerFourStones = []
                    , isBasicGame = False
                    , otherPlayerFourStones = []
                    , state = WaitingForMove
                    , whiteCount = { captured = 0, own = 12 }
                    , whiteGipfCount = 3
                    , whitePlayedNonGipf = True
                    }
        , test "Game with some remove Action" <|
            \_ ->
                Expect.equal game1
                    (Just
                        { actionHistory = [ MoveAction { color = Black, direction = { from = ( 0, 4 ), to = ( 1, 4 ) }, kind = Regular }, MoveAction { color = White, direction = { from = ( 6, 2 ), to = ( 5, 2 ) }, kind = Regular }, MoveAction { color = Black, direction = { from = ( 5, 8 ), to = ( 5, 7 ) }, kind = Regular }, MoveAction { color = White, direction = { from = ( 8, 6 ), to = ( 7, 6 ) }, kind = Regular }, MoveAction { color = Black, direction = { from = ( 1, 5 ), to = ( 2, 5 ) }, kind = Regular }, MoveAction { color = White, direction = { from = ( 1, 5 ), to = ( 2, 5 ) }, kind = Regular }, MoveAction { color = Black, direction = { from = ( 6, 8 ), to = ( 6, 7 ) }, kind = Regular }, MoveAction { color = White, direction = { from = ( 8, 6 ), to = ( 7, 6 ) }, kind = Regular }, MoveAction { color = Black, direction = { from = ( 8, 7 ), to = ( 7, 6 ) }, kind = Regular }, MoveAction { color = White, direction = { from = ( 8, 6 ), to = ( 7, 6 ) }, kind = Regular }, MoveAction { color = Black, direction = { from = ( 0, 4 ), to = ( 1, 4 ) }, kind = Regular }, MoveAction { color = White, direction = { from = ( 8, 7 ), to = ( 7, 6 ) }, kind = Regular }, MoveAction { color = Black, direction = { from = ( 2, 6 ), to = ( 2, 5 ) }, kind = Gipf }, MoveAction { color = White, direction = { from = ( 8, 5 ), to = ( 7, 5 ) }, kind = Gipf }, MoveAction { color = Black, direction = { from = ( 2, 6 ), to = ( 2, 5 ) }, kind = Gipf }, MoveAction { color = White, direction = { from = ( 8, 5 ), to = ( 7, 5 ) }, kind = Gipf }, MoveAction { color = Black, direction = { from = ( 1, 5 ), to = ( 2, 5 ) }, kind = Gipf }, MoveAction { color = White, direction = { from = ( 8, 6 ), to = ( 7, 5 ) }, kind = Gipf } ]
                        , blackCount = { captured = 0, own = 6 }
                        , blackGipfCount = 3
                        , blackPlayedNonGipf = True
                        , board = Dict.fromList [ ( ( 1, 4 ), { color = Black, coord = ( 1, 4 ), kind = Regular } ), ( ( 2, 3 ), { color = Black, coord = ( 2, 3 ), kind = Gipf } ), ( ( 2, 4 ), { color = Black, coord = ( 2, 4 ), kind = Regular } ), ( ( 2, 5 ), { color = Black, coord = ( 2, 5 ), kind = Regular } ), ( ( 3, 4 ), { color = Black, coord = ( 3, 4 ), kind = Gipf } ), ( ( 3, 5 ), { color = White, coord = ( 3, 5 ), kind = Regular } ), ( ( 4, 5 ), { color = Black, coord = ( 4, 5 ), kind = Gipf } ), ( ( 4, 6 ), { color = White, coord = ( 4, 6 ), kind = Regular } ), ( ( 5, 2 ), { color = White, coord = ( 5, 2 ), kind = Regular } ), ( ( 5, 4 ), { color = White, coord = ( 5, 4 ), kind = Gipf } ), ( ( 5, 5 ), { color = White, coord = ( 5, 5 ), kind = Gipf } ), ( ( 5, 6 ), { color = Black, coord = ( 5, 6 ), kind = Regular } ), ( ( 5, 7 ), { color = Black, coord = ( 5, 7 ), kind = Regular } ), ( ( 6, 5 ), { color = White, coord = ( 6, 5 ), kind = Regular } ), ( ( 6, 6 ), { color = White, coord = ( 6, 6 ), kind = Regular } ), ( ( 6, 7 ), { color = Black, coord = ( 6, 7 ), kind = Regular } ), ( ( 7, 5 ), { color = White, coord = ( 7, 5 ), kind = Gipf } ), ( ( 7, 6 ), { color = White, coord = ( 7, 6 ), kind = Regular } ) ]
                        , currentColor = Black -- the color doesn't unchange until Black removes their pieces
                        , currentKind = Regular
                        , currentPlayerFourStones = [ [ { color = Black, coord = ( 2, 3 ), kind = Gipf }, { color = Black, coord = ( 3, 4 ), kind = Gipf }, { color = Black, coord = ( 4, 5 ), kind = Gipf }, { color = Black, coord = ( 5, 6 ), kind = Regular }, { color = Black, coord = ( 6, 7 ), kind = Regular } ] ]
                        , isBasicGame = False
                        , otherPlayerFourStones = []
                        , state = WaitingForRemove
                        , whiteCount = { captured = 0, own = 6 }
                        , whiteGipfCount = 3
                        , whitePlayedNonGipf = True
                        }
                    )
        ]


actionTest : Test
actionTest =
    describe "Action tests"
        [ test "remove pieces from game1" <|
            \_ ->
                Expect.equal
                    (performAction (Just (RemoveAction [ ( 5, 6 ), ( 6, 7 ) ])) game1)
                    (Just
                        { actionHistory = [ RemoveAction [ ( 5, 6 ), ( 6, 7 ) ], MoveAction { color = Black, direction = { from = ( 0, 4 ), to = ( 1, 4 ) }, kind = Regular }, MoveAction { color = White, direction = { from = ( 6, 2 ), to = ( 5, 2 ) }, kind = Regular }, MoveAction { color = Black, direction = { from = ( 5, 8 ), to = ( 5, 7 ) }, kind = Regular }, MoveAction { color = White, direction = { from = ( 8, 6 ), to = ( 7, 6 ) }, kind = Regular }, MoveAction { color = Black, direction = { from = ( 1, 5 ), to = ( 2, 5 ) }, kind = Regular }, MoveAction { color = White, direction = { from = ( 1, 5 ), to = ( 2, 5 ) }, kind = Regular }, MoveAction { color = Black, direction = { from = ( 6, 8 ), to = ( 6, 7 ) }, kind = Regular }, MoveAction { color = White, direction = { from = ( 8, 6 ), to = ( 7, 6 ) }, kind = Regular }, MoveAction { color = Black, direction = { from = ( 8, 7 ), to = ( 7, 6 ) }, kind = Regular }, MoveAction { color = White, direction = { from = ( 8, 6 ), to = ( 7, 6 ) }, kind = Regular }, MoveAction { color = Black, direction = { from = ( 0, 4 ), to = ( 1, 4 ) }, kind = Regular }, MoveAction { color = White, direction = { from = ( 8, 7 ), to = ( 7, 6 ) }, kind = Regular }, MoveAction { color = Black, direction = { from = ( 2, 6 ), to = ( 2, 5 ) }, kind = Gipf }, MoveAction { color = White, direction = { from = ( 8, 5 ), to = ( 7, 5 ) }, kind = Gipf }, MoveAction { color = Black, direction = { from = ( 2, 6 ), to = ( 2, 5 ) }, kind = Gipf }, MoveAction { color = White, direction = { from = ( 8, 5 ), to = ( 7, 5 ) }, kind = Gipf }, MoveAction { color = Black, direction = { from = ( 1, 5 ), to = ( 2, 5 ) }, kind = Gipf }, MoveAction { color = White, direction = { from = ( 8, 6 ), to = ( 7, 5 ) }, kind = Gipf } ]
                        , blackCount = { captured = 0, own = 8 }
                        , blackGipfCount = 3
                        , blackPlayedNonGipf = True
                        , board = Dict.fromList [ ( ( 1, 4 ), { color = Black, coord = ( 1, 4 ), kind = Regular } ), ( ( 2, 3 ), { color = Black, coord = ( 2, 3 ), kind = Gipf } ), ( ( 2, 4 ), { color = Black, coord = ( 2, 4 ), kind = Regular } ), ( ( 2, 5 ), { color = Black, coord = ( 2, 5 ), kind = Regular } ), ( ( 3, 4 ), { color = Black, coord = ( 3, 4 ), kind = Gipf } ), ( ( 3, 5 ), { color = White, coord = ( 3, 5 ), kind = Regular } ), ( ( 4, 5 ), { color = Black, coord = ( 4, 5 ), kind = Gipf } ), ( ( 4, 6 ), { color = White, coord = ( 4, 6 ), kind = Regular } ), ( ( 5, 2 ), { color = White, coord = ( 5, 2 ), kind = Regular } ), ( ( 5, 4 ), { color = White, coord = ( 5, 4 ), kind = Gipf } ), ( ( 5, 5 ), { color = White, coord = ( 5, 5 ), kind = Gipf } ), ( ( 5, 7 ), { color = Black, coord = ( 5, 7 ), kind = Regular } ), ( ( 6, 5 ), { color = White, coord = ( 6, 5 ), kind = Regular } ), ( ( 6, 6 ), { color = White, coord = ( 6, 6 ), kind = Regular } ), ( ( 7, 5 ), { color = White, coord = ( 7, 5 ), kind = Gipf } ), ( ( 7, 6 ), { color = White, coord = ( 7, 6 ), kind = Regular } ) ]
                        , currentColor = White
                        , currentKind = Regular
                        , currentPlayerFourStones = []
                        , isBasicGame = False
                        , otherPlayerFourStones = []
                        , state = WaitingForMove
                        , whiteCount = { captured = 0, own = 6 }
                        , whiteGipfCount = 3
                        , whitePlayedNonGipf = True
                        }
                    )
        , test "reconstruct a game with 4 pieces in a row" <|
            \_ ->
                let
                    g =
                        stringToGameWithDefault "GWi3-h3 GKb6-c6 GWi2-h3 GKc7-c6 GWi2-h3 GKc7-c6 Wi4-h4 Ka5-b5 Wi3-h4 Ki4-h4 Wi3-h4 Kg7-g6 Wb6-c6 Kb6-c6 Wi3-h4 Kf8-f7 Wg1-f2 Ka5-b5 xf6,g6 Wf1-f2"
                in
                Expect.equal g.state WaitingForRemove
        ]
