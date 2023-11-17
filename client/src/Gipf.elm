module Gipf exposing (..)

import Dict exposing (Dict)
import Regex
import Tools exposing (..)


type alias Coord =
    ( Int, Int )


type Color
    = Black
    | White


type Kind
    = Regular
    | Gipf


type alias Piece =
    { coord : Coord, color : Color, kind : Kind }


type alias Direction =
    { from : Coord, to : Coord }


type alias BoardPieces =
    Dict Coord Piece


piecesToBoard : List Piece -> BoardPieces
piecesToBoard pieces =
    List.map (\piece -> ( piece.coord, piece )) pieces
        |> Dict.fromList


standardStartingBoard : BoardPieces
standardStartingBoard =
    piecesToBoard
        [ Piece ( 4, 1 ) Black Gipf
        , Piece ( 7, 7 ) Black Gipf
        , Piece ( 1, 4 ) Black Gipf
        , Piece ( 4, 7 ) White Gipf
        , Piece ( 7, 4 ) White Gipf
        , Piece ( 1, 1 ) White Gipf
        ]


boardToPieces : BoardPieces -> List Piece
boardToPieces board =
    Dict.values board


boardPointQ : Coord -> Bool
boardPointQ ( x, y ) =
    x >= 0 && x <= 8 && Basics.max (x - 4) 0 <= y && y <= Basics.min (4 + x) 8


interiorBoardPointQ : Coord -> Bool
interiorBoardPointQ ( x, y ) =
    x >= 1 && x <= 7 && y >= 1 && y <= 7 && abs (x - y) < 4


edgeBoardPointQ : Coord -> Bool
edgeBoardPointQ ( x, y ) =
    x == 0 || x == 8 || y == 0 || y == 8 || abs (x - y) == 4


boardPoints : List Coord
boardPoints =
    List.concatMap (\x -> List.map (\y -> ( x, y )) (List.range 0 8)) (List.range 0 8)
        |> List.filter boardPointQ


interiorBoardPoints : List Coord
interiorBoardPoints =
    List.filter interiorBoardPointQ boardPoints


edgeBoardPoints : List Coord
edgeBoardPoints =
    List.filter edgeBoardPointQ boardPoints


neighbors : Coord -> List Coord
neighbors point =
    let
        adjustments =
            [ ( 0, 1 )
            , ( 0, -1 )
            , ( 1, 0 )
            , ( -1, 0 )
            , ( 1, 1 )
            , ( -1, -1 )
            ]

        addPoints ( x, y ) ( ax, ay ) =
            ( x + ax, y + ay )

        allNeighbors =
            List.map (addPoints point) adjustments
    in
    List.filter interiorBoardPointQ allNeighbors


stepVector : Coord -> Coord -> Coord
stepVector ( x1, y1 ) ( x2, y2 ) =
    let
        ( vx, vy ) =
            ( x2 - x1, y2 - y1 )

        maxAbs =
            max (abs vx) (abs vy)
    in
    if maxAbs == 0 then
        ( 0, 0 )

    else
        ( vx // maxAbs, vy // maxAbs )


coordinatesSlice : Coord -> Coord -> List Coord
coordinatesSlice ( x1, y1 ) ( x2, y2 ) =
    let
        generatePoints i =
            let
                ( sx, sy ) =
                    stepVector ( x1, y1 ) ( x2, y2 )
            in
            ( x1 + sx * i, y1 + sy * i )
    in
    List.range 1 7
        |> List.map generatePoints
        |> List.filter interiorBoardPointQ


boardSlice : BoardPieces -> Direction -> List (Maybe Piece)
boardSlice boardPieces move =
    dictSlice boardPieces (coordinatesSlice move.from move.to)


allMoves : List Direction
allMoves =
    List.concatMap
        (\point ->
            List.map
                (\neighbor -> { from = point, to = neighbor })
                (neighbors point)
        )
        edgeBoardPoints


movePossibleQ : BoardPieces -> Direction -> Bool
movePossibleQ boardPieces move =
    anyKeyMissing
        boardPieces
        (coordinatesSlice move.from move.to)


availableMoves : BoardPieces -> List Direction
availableMoves boardPieces =
    List.filter
        (movePossibleQ boardPieces)
        allMoves


nameToCoord : String -> Maybe Coord
nameToCoord name =
    let
        x =
            String.left 1 name
                |> String.toList
                |> List.head
                |> Maybe.map Char.toCode
                |> Maybe.map (\code -> code - Char.toCode 'a')
                |> Maybe.withDefault -1

        y =
            String.slice 1 2 name
                |> String.toInt
                |> Maybe.map (\num -> num - 1 + max 0 (x - 4))
                |> Maybe.withDefault -1

        c =
            ( x, y )
    in
    if boardPointQ c then
        Just c

    else
        Nothing


coordToName : Coord -> String
coordToName ( x, y ) =
    let
        xChar =
            Char.fromCode (x + Char.toCode 'a')

        adjustedY =
            y + 1 - max 0 (x - 4)
    in
    String.fromChar xChar ++ String.fromInt adjustedY


removePieces : List Coord -> BoardPieces -> BoardPieces
removePieces coords boardPieces =
    List.foldl
        (\coord board -> Dict.remove coord board)
        boardPieces
        coords


insertPiece : Piece -> BoardPieces -> BoardPieces
insertPiece piece board =
    Dict.insert piece.coord piece board


insertPiecesWithVector : List Piece -> Coord -> BoardPieces -> BoardPieces
insertPiecesWithVector pieces vec board =
    let
        addCoords p ( vx, vy ) =
            let
                ( x, y ) =
                    p.coord
            in
            Piece ( x + vx, y + vy ) p.color p.kind
    in
    List.foldl
        (\p b -> insertPiece (addCoords p vec) b)
        board
        pieces


insertPieceWithMove : Direction -> Color -> Kind -> BoardPieces -> Maybe BoardPieces
insertPieceWithMove move color kind boardPieces =
    if movePossibleQ boardPieces move then
        let
            ( x1, y1 ) =
                move.from

            ( vx, vy ) =
                stepVector move.from move.to

            coordSlice =
                coordinatesSlice move.from move.to

            slice =
                dictSlice boardPieces coordSlice

            sliceWithoutNothing =
                largestPrefixWithoutNothing slice

            coordsSliceWithoutNothing =
                List.take (List.length sliceWithoutNothing) coordSlice
        in
        Just
            (boardPieces
                |> removePieces coordsSliceWithoutNothing
                |> insertPiecesWithVector sliceWithoutNothing ( vx, vy )
                |> Dict.insert ( x1 + vx, y1 + vy ) (Piece ( x1 + vx, y1 + vy ) color kind)
            )

    else
        Nothing


piecesToString : List Piece -> String
piecesToString pieces =
    List.map
        (\p ->
            (if p.color == Black && p.kind == Gipf then
                "GK"

             else if p.color == Black && p.kind == Regular then
                "K"

             else if p.color == White && p.kind == Gipf then
                "GW"

             else
                "W"
            )
                ++ coordToName p.coord
        )
        pieces
        |> List.sort
        |> String.join " "


boardToString : BoardPieces -> String
boardToString board =
    piecesToString (Dict.values board)


stringToPiece : String -> Maybe Piece
stringToPiece s =
    let
        g =
            String.left 1 s
    in
    if g == "" then
        Nothing

    else if g == "G" then
        Maybe.map
            (\p -> { p | kind = Gipf })
            (stringToPiece
                (String.dropLeft 1 s)
            )

    else
        let
            k =
                String.left 1 s

            c =
                nameToCoord (String.dropLeft 1 s)
        in
        if k == "K" || k == "W" then
            Maybe.map
                (\cc ->
                    { coord = cc
                    , color =
                        if k == "K" then
                            Black

                        else
                            White
                    , kind = Regular
                    }
                )
                c

        else
            Nothing


stringToPieces : String -> Maybe (List Piece)
stringToPieces str =
    maybeList (List.map stringToPiece (String.split " " str))


stringToBoard : String -> Maybe BoardPieces
stringToBoard str =
    Maybe.map piecesToBoard (stringToPieces str)


stringToBoardWithDefault : String -> BoardPieces
stringToBoardWithDefault str =
    Maybe.withDefault standardStartingBoard (stringToBoard str)



-- Detecting 4 in a row


sameColorListQ : List (Maybe Piece) -> Bool
sameColorListQ list =
    case list of
        [] ->
            True

        Nothing :: _ ->
            False

        (Just p) :: xs ->
            List.all
                (\x ->
                    case x of
                        Nothing ->
                            False

                        Just pp ->
                            p.color == pp.color
                )
                xs


allLines : List Direction
allLines =
    [ { from = ( 0, 0 ), to = ( 1, 1 ) }
    , { from = ( 1, 0 ), to = ( 1, 1 ) }
    , { from = ( 1, 0 ), to = ( 2, 1 ) }
    , { from = ( 2, 0 ), to = ( 2, 1 ) }
    , { from = ( 2, 0 ), to = ( 3, 1 ) }
    , { from = ( 3, 0 ), to = ( 3, 1 ) }
    , { from = ( 3, 0 ), to = ( 4, 1 ) }
    , { from = ( 4, 0 ), to = ( 4, 1 ) }
    , { from = ( 0, 1 ), to = ( 1, 2 ) }
    , { from = ( 0, 1 ), to = ( 1, 1 ) }
    , { from = ( 0, 2 ), to = ( 1, 3 ) }
    , { from = ( 0, 2 ), to = ( 1, 2 ) }
    , { from = ( 0, 3 ), to = ( 1, 4 ) }
    , { from = ( 0, 3 ), to = ( 1, 3 ) }
    , { from = ( 0, 4 ), to = ( 1, 4 ) }
    , { from = ( 1, 5 ), to = ( 2, 5 ) }
    , { from = ( 1, 5 ), to = ( 1, 4 ) }
    , { from = ( 2, 6 ), to = ( 3, 6 ) }
    , { from = ( 2, 6 ), to = ( 2, 5 ) }
    , { from = ( 3, 7 ), to = ( 4, 7 ) }
    , { from = ( 3, 7 ), to = ( 3, 6 ) }
    ]


connectedGroupOfFour : List (Maybe Piece) -> Maybe (List Piece)
connectedGroupOfFour slice =
    sublistsOfFour slice
        |> List.filter (\( _, list ) -> sameColorListQ list)
        |> List.head
        |> Maybe.map (\( i, _ ) -> extendSublistWithJustItems slice i)


connectedGroupsOfFour : BoardPieces -> List (List Piece)
connectedGroupsOfFour b =
    List.map (\line -> boardSlice b line) allLines
        |> List.map connectedGroupOfFour
        |> List.filterMap identity



-- Game state


type alias PiecesCount =
    { own : Int
    , captured : Int
    }


type alias Move =
    { direction : Direction, color : Color, kind : Kind }


type GameState
    = WaitingForMove
    | WaitingForRemove
    | BlackWon
    | WhiteWon


type Action
    = MoveAction Move
    | RemoveAction (List Coord)


type alias Game =
    { board : BoardPieces
    , currentKind : Kind
    , currentColor : Color
    , state : GameState
    , blackCount : PiecesCount
    , whiteCount : PiecesCount
    , blackGipfCount : Int
    , whiteGipfCount : Int
    , isBasicGame : Bool
    , blackPlayedNonGipf : Bool
    , whitePlayedNonGipf : Bool
    , currentPlayerFourStones : List (List Piece)
    , otherPlayerFourStones : List (List Piece)
    , actionHistory : List Action -- from newest to oldest
    }


invalidMoveQ : Move -> Game -> Bool
invalidMoveQ move game =
    (game.currentColor /= move.color)
        || (game.state /= WaitingForMove)
        || -- in a properly constructed game, the above should be enough, but just in case we have 2 more conditions
           ((move.color == White && game.whiteCount.own == 0)
                || (move.color == Black && game.blackCount.own == 0)
           )
        || not (List.isEmpty game.currentPlayerFourStones || List.isEmpty game.otherPlayerFourStones)
        || -- in a basic game, you can't play a Gipf piece
           (game.isBasicGame && move.kind == Gipf)
        || -- once you played a non-Gipf piece, you can't play a Gipf piece anymore
           ((game.blackPlayedNonGipf && move.color == Black && move.kind == Gipf)
                || (game.whitePlayedNonGipf && move.color == White && move.kind == Gipf)
           )
        || not (movePossibleQ game.board move.direction)


dominantColor : List Piece -> Color
dominantColor group =
    let
        blackCount =
            count group (\p -> p.color == Black)

        whiteCount =
            count group (\p -> p.color == White)
    in
    if blackCount > whiteCount then
        Black

    else
        White


currentAndOtherFourStones : BoardPieces -> Color -> ( List (List Piece), List (List Piece) )
currentAndOtherFourStones board color =
    splitByPredicate
        (\group -> dominantColor group == color)
        (connectedGroupsOfFour board)


performMove : Move -> Game -> Maybe Game
performMove move game =
    if invalidMoveQ move game then
        Nothing

    else
        Maybe.map
            (\newBoard ->
                let
                    ( currentFourStones, otherFourStones ) =
                        currentAndOtherFourStones newBoard move.color

                    updatedGame =
                        if game.currentColor == White then
                            { game
                                | currentColor = Black
                                , currentKind =
                                    if not game.isBasicGame then
                                        Regular

                                    else if game.blackPlayedNonGipf then
                                        Regular

                                    else
                                        Gipf
                                , whiteCount =
                                    { own = game.whiteCount.own - 1
                                    , captured = game.whiteCount.captured
                                    }
                                , whitePlayedNonGipf = game.whitePlayedNonGipf || move.kind == Regular
                            }

                        else
                            { game
                                | currentColor = White
                                , currentKind =
                                    if not game.isBasicGame then
                                        Regular

                                    else if game.whitePlayedNonGipf then
                                        Regular

                                    else
                                        Gipf
                                , blackCount =
                                    { own = game.blackCount.own - 1
                                    , captured = game.blackCount.captured
                                    }
                                , blackPlayedNonGipf = game.blackPlayedNonGipf || move.kind == Regular
                            }
                in
                { updatedGame
                    | board = newBoard
                    , currentPlayerFourStones = currentFourStones
                    , otherPlayerFourStones = otherFourStones
                    , actionHistory = MoveAction move :: game.actionHistory
                    , state =
                        if not (List.isEmpty currentFourStones || List.isEmpty otherFourStones) then
                            WaitingForMove

                        else if game.currentColor == Black && game.whiteCount.own == 0 then
                            BlackWon

                        else if game.currentColor == White && game.blackCount.own == 0 then
                            WhiteWon

                        else
                            WaitingForMove
                }
            )
            (insertPieceWithMove move.direction move.color move.kind game.board)


removeInvalidQ : List Piece -> Game -> Bool
removeInvalidQ piece game =
    let
        remaining =
            removeElementsFromOneOfSupersets (game.currentPlayerFourStones ++ game.currentPlayerFourStones) piece

        removingCurrent =
            isSubsetOfAny game.currentPlayerFourStones piece

        removingOther =
            isSubsetOfAny game.otherPlayerFourStones piece
    in
    (game.state /= WaitingForMove)
        || List.isEmpty piece
        || -- the above should be enough, but just in case we have 1 more conditions
           (game.currentPlayerFourStones == [] && game.otherPlayerFourStones == [])
        || -- pieces are not the ones that need to be removed
           not (removingCurrent || removingOther)
        || -- current player removes their connected stones first
           (game.currentPlayerFourStones /= [] && removingOther)
        || -- stones remained after removal by current player contain other player stones
           (removingCurrent && List.any (\p -> p.color /= game.currentColor) remaining)
        || -- stones remained after removal by other player contain current player stones
           (removingOther && List.any (\p -> p.color == game.currentColor) remaining)
        || -- remaining stones contain regular stones of either player
           List.any (\p -> p.kind == Regular) remaining


countPieces : List Piece -> ( Int, Int )
countPieces pieces =
    -- count number of white and black pieces (count gipf pieces as 2)
    let
        countPiece p ( w, b ) =
            if p.color == White then
                ( w
                    + (if p.kind == Gipf then
                        2

                       else
                        1
                      )
                , b
                )

            else
                ( w
                , b
                    + (if p.kind == Gipf then
                        2

                       else
                        1
                      )
                )
    in
    List.foldl countPiece ( 0, 0 ) pieces


performRemove : List Coord -> Game -> Maybe Game
performRemove coords game =
    -- TODO: in situation where there are 4 adjacent gipf pieces in a row (and no other adjacent pieces),
    -- which can easily happen at the start of the tournament game, performRemove will fail
    -- to proceed to the next state, because it will think that the player need to remove
    -- some pieces, while in fact they don't need to remove anything. We need to have some way to
    -- record that the player decide to keep the 4 gipf pieces, and then proceed to the next state.
    let
        maybePieces =
            dictSlice game.board coords
                |> maybeList
    in
    case maybePieces of
        Just pieces ->
            if removeInvalidQ pieces game then
                Nothing

            else
                let
                    newPieces =
                        removeElements pieces (boardToPieces game.board)

                    newBoard =
                        piecesToBoard newPieces

                    ( currentFourStones, otherFourStones ) =
                        currentAndOtherFourStones newBoard game.currentColor

                    ( removedWhiteCount, removedBlackCount ) =
                        countPieces pieces

                    removingPlayerColor =
                        dominantColor pieces

                    updatedGame =
                        { game
                            | board = newBoard
                            , currentPlayerFourStones = currentFourStones
                            , otherPlayerFourStones = otherFourStones
                            , actionHistory = RemoveAction coords :: game.actionHistory
                            , whiteCount =
                                { own =
                                    if removingPlayerColor == White then
                                        game.whiteCount.own + removedWhiteCount

                                    else
                                        game.whiteCount.own
                                , captured =
                                    if removingPlayerColor == White then
                                        game.whiteCount.captured + removedBlackCount

                                    else
                                        game.whiteCount.captured
                                }
                            , blackCount =
                                { own =
                                    if removingPlayerColor == Black then
                                        game.blackCount.own + removedBlackCount

                                    else
                                        game.blackCount.own
                                , captured =
                                    if removingPlayerColor == Black then
                                        game.blackCount.captured + removedWhiteCount

                                    else
                                        game.blackCount.captured
                                }
                            , whiteGipfCount = game.whiteGipfCount - count pieces (\p -> p.color == White && p.kind == Gipf)
                            , blackGipfCount = game.whiteGipfCount - count pieces (\p -> p.color == White && p.kind == Gipf)
                        }

                    newState =
                        if not (List.isEmpty updatedGame.currentPlayerFourStones || List.isEmpty updatedGame.otherPlayerFourStones) then
                            WaitingForRemove

                        else if
                            (updatedGame.currentColor == Black && updatedGame.whiteCount.own == 0)
                                || (updatedGame.whiteGipfCount == 0)
                        then
                            BlackWon

                        else if
                            (updatedGame.currentColor == White && updatedGame.blackCount.own == 0)
                                || (updatedGame.blackGipfCount == 0)
                        then
                            WhiteWon

                        else
                            WaitingForMove
                in
                Just
                    { updatedGame
                        | state = newState
                        , currentColor =
                            if newState == WaitingForMove then
                                if game.currentColor == White then
                                    Black

                                else
                                    White

                            else
                                updatedGame.currentColor
                    }

        Nothing ->
            Nothing


performAction : Action -> Game -> Maybe Game
performAction action game =
    case action of
        MoveAction move ->
            performMove move game

        RemoveAction coords ->
            performRemove coords game



-- String conversions


moveActionRx : Regex.Regex
moveActionRx =
    Maybe.withDefault Regex.never <|
        Regex.fromString "^(G?)([KW]?)([a-i][1-9])-([a-i][1-9])$"


stringToMove : String -> Maybe Move
stringToMove str =
    case Regex.find moveActionRx str of
        [] ->
            Nothing

        match :: _ ->
            let
                kind =
                    case match.submatches |> List.head |> Maybe.withDefault Nothing of
                        Just "G" ->
                            Just Gipf

                        _ ->
                            Just Regular

                color =
                    case match.submatches |> List.drop 1 |> List.head |> Maybe.withDefault Nothing of
                        Just "K" ->
                            Just Black

                        _ ->
                            Just White

                fromCoord =
                    match.submatches
                        |> List.drop 2
                        |> List.head
                        |> Maybe.andThen (Maybe.andThen nameToCoord)

                toCoord =
                    match.submatches
                        |> List.drop 3
                        |> List.head
                        |> Maybe.andThen (Maybe.andThen nameToCoord)
            in
            case ( ( kind, color ), ( fromCoord, toCoord ) ) of
                ( ( Just k, Just c ), ( Just from, Just to ) ) ->
                    Just { direction = Direction from to, color = c, kind = k }

                _ ->
                    Nothing


removeActionRx : Regex.Regex
removeActionRx =
    Maybe.withDefault Regex.never <|
        Regex.fromString "^x[a-i][1-9](,[a-i][1-9])*$"


stringToRemove : String -> Maybe (List Coord)
stringToRemove str =
    if Regex.contains removeActionRx str then
        str
            |> String.dropLeft 1
            |> String.split ","
            |> List.map nameToCoord
            |> maybeList

    else
        Nothing


stringToAction : String -> Maybe Action
stringToAction str =
    if String.left 1 str == "x" then
        Maybe.map RemoveAction (stringToRemove str)

    else
        Maybe.map MoveAction (stringToMove str)


stringToActions : String -> Maybe (List Action)
stringToActions str =
    maybeList (List.map stringToAction (String.split " " str))


emptyGame : Game
emptyGame =
    { board = Dict.empty
    , currentKind = Gipf
    , currentColor = White
    , state = WaitingForMove
    , blackCount = { own = 18, captured = 0 }
    , whiteCount = { own = 18, captured = 0 }
    , blackGipfCount = 0
    , whiteGipfCount = 0
    , isBasicGame = True
    , blackPlayedNonGipf = False
    , whitePlayedNonGipf = False
    , currentPlayerFourStones = []
    , otherPlayerFourStones = []
    , actionHistory = []
    }


stringToGame : String -> Maybe Game
stringToGame str =
    case stringToActions str of
        Just actions ->
            maybeFoldr (\g a -> performAction g a) emptyGame actions

        Nothing ->
            Nothing
