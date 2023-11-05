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



-- boardToPieces takes a board and returns a list of pieces where each piece is a tuple of (key, value)


boardToPieces : BoardPieces -> List Board.Piece
boardToPieces board =
    Dict.toList board |> List.map (\( ( x, y ), kind ) -> Board.Piece { x = x, y = y } kind)


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
