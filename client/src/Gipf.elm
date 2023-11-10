module Gipf exposing (..)

import Dict exposing (Dict)
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


type alias Move =
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


boardSlice : BoardPieces -> Move -> List (Maybe Piece)
boardSlice boardPieces move =
    dictSlice boardPieces (coordinatesSlice move.from move.to)


allMoves : List Move
allMoves =
    List.concatMap
        (\point ->
            List.map
                (\neighbor -> { from = point, to = neighbor })
                (neighbors point)
        )
        edgeBoardPoints


movePossibleQ : BoardPieces -> Move -> Bool
movePossibleQ boardPieces move =
    anyKeyMissing
        boardPieces
        (coordinatesSlice move.from move.to)


availableMoves : BoardPieces -> List Move
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


insertPieces : List Piece -> BoardPieces -> BoardPieces
insertPieces pieces board =
    List.foldl (\p b -> Dict.insert p.coord p b) board pieces


addCoords : Coord -> Coord -> Coord
addCoords ( x, y ) ( vx, vy ) =
    ( x + vx, y + vy )


insertWithVector : List Piece -> Coord -> BoardPieces -> BoardPieces
insertWithVector pieces vec board =
    insertPieces
        (List.map (\p -> Piece (addCoords p.coord vec) p.color p.kind) pieces)
        board


performMove : Move -> Color -> Kind -> BoardPieces -> Maybe BoardPieces
performMove move color kind boardPieces =
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
                |> insertWithVector sliceWithoutNothing ( vx, vy )
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
                "WK"

             else if p.color == White && p.kind == Gipf then
                "W"

             else
                "W"
            )
                ++ coordToName p.coord
        )
        pieces
        |> List.sort
        |> String.join ""


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


addStringToBoard : String -> BoardPieces -> Maybe BoardPieces
addStringToBoard s b =
    Maybe.map
        (\p ->
            Dict.insert p.coord p b
        )
        (stringToPiece s)


stringToBoard : String -> Maybe BoardPieces
stringToBoard str =
    List.foldl
        (\s b ->
            Maybe.andThen
                (\bb ->
                    addStringToBoard s bb
                )
                b
        )
        (Just Dict.empty)
        (String.split " " str)


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


allLines : List Move
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
