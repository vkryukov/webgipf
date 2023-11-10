module Gipf exposing (..)

import Dict exposing (Dict)
import Tools exposing (..)


type alias Coord =
    ( Int, Int )


type Color
    = Black
    | White


type PieceKind
    = Regular
    | Gipf


type alias Kind =
    { color : Color
    , kind : PieceKind
    }


type alias Piece =
    { coord : Coord, kind : Kind }


type alias Move =
    { from : Coord, to : Coord }


type alias BoardPieces =
    Dict Coord Kind


standardStartingBoard : BoardPieces
standardStartingBoard =
    Dict.fromList
        [ ( ( 4, 1 ), Kind Black Gipf )
        , ( ( 7, 7 ), Kind Black Gipf )
        , ( ( 1, 4 ), Kind Black Gipf )
        , ( ( 4, 7 ), Kind White Gipf )
        , ( ( 7, 4 ), Kind White Gipf )
        , ( ( 1, 1 ), Kind White Gipf )
        ]


boardToPieces : BoardPieces -> List Piece
boardToPieces board =
    Dict.toList board |> List.map (\( ( x, y ), kind ) -> Piece ( x, y ) kind)


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
    let
        cs =
            coordinatesSlice move.from move.to

        k =
            dictSlice boardPieces cs
    in
    List.map2 (\key value -> Maybe.map (\k2 -> { coord = key, kind = k2 }) value)
        cs
        k


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


removeCoords : List ( Int, Int ) -> BoardPieces -> BoardPieces
removeCoords coords boardPieces =
    List.foldl
        (\coord board -> Dict.remove coord board)
        boardPieces
        coords


insertWithVector : List ( Int, Int ) -> List Kind -> Coord -> BoardPieces -> BoardPieces
insertWithVector coords kinds ( vx, vy ) boardPieces =
    let
        zippedList =
            List.map2 (\coord kind -> ( coord, kind )) coords kinds
    in
    List.foldl
        (\( ( x, y ), kind ) board ->
            let
                newCoord =
                    ( x + vx, y + vy )
            in
            Dict.insert newCoord kind board
        )
        boardPieces
        zippedList


performMove : Move -> Kind -> BoardPieces -> Maybe BoardPieces
performMove move kind boardPieces =
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
                |> removeCoords coordsSliceWithoutNothing
                |> insertWithVector coordsSliceWithoutNothing sliceWithoutNothing ( vx, vy )
                |> Dict.insert ( x1 + vx, y1 + vy ) kind
            )

    else
        Nothing


boardToString : BoardPieces -> String
boardToString boardPieces =
    Dict.toList boardPieces
        |> List.map
            (\( ( x, y ), { color, kind } ) ->
                (if color == Black then
                    if kind == Regular then
                        "K"

                    else
                        "GK"

                 else if kind == Regular then
                    "W"

                 else
                    "GW"
                )
                    ++ coordToName ( x, y )
            )
        |> List.sort
        |> String.join " "


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
            (\p ->
                { p
                    | kind =
                        if p.kind.color == Black then
                            Kind Black Gipf

                        else
                            Kind White Gipf
                }
            )
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
                    , kind =
                        if k == "K" then
                            Kind Black Regular

                        else
                            Kind White Regular
                    }
                )
                c

        else
            Nothing


addStringToBoard : String -> BoardPieces -> Maybe BoardPieces
addStringToBoard s b =
    Maybe.map
        (\p ->
            Dict.insert p.coord p.kind b
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



-- Detecting 4 in a row


sameColorQ : Kind -> Kind -> Bool
sameColorQ k1 k2 =
    k1.color == k2.color


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
                            sameColorQ p.kind pp.kind
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
        |> Maybe.map (\( i, list ) -> extendSublistWithJustItems list i)


connectedGroupsOfFour : BoardPieces -> List (List Piece)
connectedGroupsOfFour b =
    List.map (\line -> boardSlice b line) allLines
        |> List.map connectedGroupOfFour
        |> List.filterMap identity
