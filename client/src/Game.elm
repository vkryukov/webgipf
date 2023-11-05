module Game exposing (..)

import Board
import Browser
import Dict exposing (Dict)
import Html exposing (Html)


type alias BoardPieces =
    Dict ( Int, Int ) Board.Kind


type alias Model =
    { boardDict : BoardPieces
    , board : Board.Model
    }


startingBoard : BoardPieces
startingBoard =
    Dict.fromList
        [ ( ( 4, 1 ), Board.BlackGipf )
        , ( ( 7, 7 ), Board.BlackGipf )
        , ( ( 1, 4 ), Board.BlackGipf )
        , ( ( 4, 7 ), Board.WhiteGipf )
        , ( ( 7, 4 ), Board.WhiteGipf )
        , ( ( 1, 1 ), Board.WhiteGipf )
        ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( initFromDict startingBoard
    , Cmd.none
    )


initFromDict : BoardPieces -> Model
initFromDict boardDict =
    { boardDict = boardDict
    , board = Board.initFromPiecesAndMoves (boardToPieces boardDict) []
    }


boardToPieces : BoardPieces -> List Board.Piece
boardToPieces board =
    Dict.toList board |> List.map (\( ( x, y ), kind ) -> Board.Piece { x = x, y = y } kind)


neighbors : Board.Coord -> List Board.Coord
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
    List.filter Board.interiorBoardPointQ allNeighbors


stepVector : Board.Coord -> Board.Coord -> Board.Coord
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


boardSlice : Board.Coord -> Board.Coord -> List Board.Coord
boardSlice p1 p2 =
    let
        generatePoints i =
            let
                step =
                    stepVector p1 p2
            in
            { x = p1.x + step.x * i, y = p1.y + step.y * i }
    in
    List.range 0 6
        |> List.map generatePoints
        |> List.filter Board.interiorBoardPointQ


type Msg
    = BoardMsg Board.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BoardMsg boardMsg ->
            let
                ( newBoard, boardCmd ) =
                    Board.update boardMsg model.board
            in
            ( { model | board = newBoard }, Cmd.map BoardMsg boardCmd )


view : Model -> Html Msg
view model =
    model.board
        |> Board.view
        |> Html.map BoardMsg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
