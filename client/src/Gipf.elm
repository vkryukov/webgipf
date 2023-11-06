module Gipf exposing (..)

import Dict exposing (Dict)


type alias Coord =
    { x : Int, y : Int }


type alias Piece =
    { coord : Coord, kind : Kind }


type Kind
    = Black
    | BlackGipf
    | White
    | WhiteGipf


type alias Move =
    { from : Coord, to : Coord }


type alias BoardPieces =
    Dict ( Int, Int ) Kind


standardStartingBoard : BoardPieces
standardStartingBoard =
    Dict.fromList
        [ ( ( 4, 1 ), BlackGipf )
        , ( ( 7, 7 ), BlackGipf )
        , ( ( 1, 4 ), BlackGipf )
        , ( ( 4, 7 ), WhiteGipf )
        , ( ( 7, 4 ), WhiteGipf )
        , ( ( 1, 1 ), WhiteGipf )
        ]


boardToPieces : BoardPieces -> List Piece
boardToPieces board =
    Dict.toList board |> List.map (\( ( x, y ), kind ) -> Piece { x = x, y = y } kind)


boardPointQ : Coord -> Bool
boardPointQ point =
    point.x >= 0 && point.x <= 8 && Basics.max (point.x - 4) 0 <= point.y && point.y <= Basics.min (4 + point.x) 8


interiorBoardPointQ : Coord -> Bool
interiorBoardPointQ point =
    point.x >= 1 && point.x <= 7 && point.y >= 1 && point.y <= 7 && abs (point.x - point.y) < 4


edgeBoardPointQ : Coord -> Bool
edgeBoardPointQ point =
    point.x == 0 || point.x == 8 || point.y == 0 || point.y == 8 || abs (point.x - point.y) == 4


boardPoints : List Coord
boardPoints =
    List.concatMap (\x -> List.map (\y -> { x = x, y = y }) (List.range 0 8)) (List.range 0 8)
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
            [ { x = 0, y = 1 }
            , { x = 0, y = -1 }
            , { x = 1, y = 0 }
            , { x = -1, y = 0 }
            , { x = 1, y = 1 }
            , { x = -1, y = -1 }
            ]

        addPoints p adjustment =
            { x = p.x + adjustment.x, y = p.y + adjustment.y }

        allNeighbors =
            List.map (addPoints point) adjustments
    in
    List.filter interiorBoardPointQ allNeighbors


stepVector : Coord -> Coord -> Coord
stepVector p1 p2 =
    let
        v =
            { x = p2.x - p1.x, y = p2.y - p1.y }

        maxAbs =
            max (abs v.x) (abs v.y)
    in
    if maxAbs == 0 then
        { x = 0, y = 0 }

    else
        { x = v.x // maxAbs, y = v.y // maxAbs }


coordinatesSlice : Coord -> Coord -> List Coord
coordinatesSlice p1 p2 =
    let
        generatePoints i =
            let
                step =
                    stepVector p1 p2
            in
            { x = p1.x + step.x * i, y = p1.y + step.y * i }
    in
    List.range 1 7
        |> List.map generatePoints
        |> List.filter interiorBoardPointQ


dictSlice : Dict comparable v -> List comparable -> List (Maybe v)
dictSlice dict keys =
    List.map (\key -> Dict.get key dict) keys


anyNothing : List (Maybe a) -> Bool
anyNothing list =
    List.any (\item -> item == Nothing) list


anyKeyMissing : Dict comparable v -> List comparable -> Bool
anyKeyMissing dict keys =
    anyNothing (dictSlice dict keys)


coordToTuples : Coord -> ( Int, Int )
coordToTuples coord =
    ( coord.x, coord.y )


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
        (List.map coordToTuples (coordinatesSlice move.from move.to))


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
            Coord x y
    in
    if boardPointQ c then
        Just c

    else
        Nothing


coordToName : Coord -> String
coordToName coord =
    let
        xChar =
            Char.fromCode (coord.x + Char.toCode 'a')

        adjustedY =
            coord.y + 1 - max 0 (coord.x - 4)
    in
    String.fromChar xChar ++ String.fromInt adjustedY


largestPrefixWithoutNothing : List (Maybe a) -> List a
largestPrefixWithoutNothing list =
    case list of
        [] ->
            []

        x :: xs ->
            case x of
                Nothing ->
                    []

                Just xx ->
                    xx :: largestPrefixWithoutNothing xs


removeCoords : List ( Int, Int ) -> BoardPieces -> BoardPieces
removeCoords coords boardPieces =
    List.foldl
        (\coord board -> Dict.remove coord board)
        boardPieces
        coords


fst : ( Int, Int ) -> Int
fst ( x, _ ) =
    x


snd : ( Int, Int ) -> Int
snd ( _, y ) =
    y


insertWithVector : List ( Int, Int ) -> List Kind -> Coord -> BoardPieces -> BoardPieces
insertWithVector coords kinds vector boardPieces =
    let
        zippedList =
            List.map2 (\coord kind -> ( coord, kind )) coords kinds
    in
    List.foldl
        (\( coord, kind ) board ->
            let
                newCoord =
                    ( fst coord + vector.x, snd coord + vector.y )
            in
            Dict.insert newCoord kind board
        )
        boardPieces
        zippedList


performMove : Move -> Kind -> BoardPieces -> Maybe BoardPieces
performMove move kind boardPieces =
    if movePossibleQ boardPieces move then
        let
            vec =
                stepVector move.from move.to

            coordSlice =
                List.map coordToTuples (coordinatesSlice move.from move.to)

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
                |> insertWithVector coordsSliceWithoutNothing sliceWithoutNothing vec
                |> Dict.insert ( move.from.x + vec.x, move.from.y + vec.y ) kind
            )

    else
        Nothing


boardToString : BoardPieces -> String
boardToString boardPieces =
    Dict.toList boardPieces
        |> List.map
            (\( ( x, y ), kind ) ->
                (case kind of
                    Black ->
                        "K"

                    BlackGipf ->
                        "GK"

                    White ->
                        "W"

                    WhiteGipf ->
                        "GW"
                )
                    ++ coordToName { x = x, y = y }
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
                        if p.kind == Black then
                            BlackGipf

                        else
                            WhiteGipf
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
                            Black

                        else
                            White
                    }
                )
                c

        else
            Nothing


addStringToBoard : String -> BoardPieces -> Maybe BoardPieces
addStringToBoard s b =
    Maybe.map
        (\p ->
            Dict.insert (coordToTuples p.coord) p.kind b
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


extendSublistWithJustItems : List (Maybe a) -> Int -> List a
extendSublistWithJustItems list start =
    let
        prefix =
            List.take start list
    in
    List.reverse (largestPrefixWithoutNothing (List.reverse prefix)) ++ largestPrefixWithoutNothing (List.drop start list)
