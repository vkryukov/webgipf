module Main exposing (..)

import Browser
import GipfBoard
import Html exposing (Html)


type alias Model =
    { board : GipfBoard.Model
    , gameId : Int
    , token : String
    }


initWithGameIdToken : Int -> String -> ( Model, Cmd Msg )
initWithGameIdToken gameId token =
    let
        ( board, _ ) =
            GipfBoard.initFromString ""
    in
    ( { board = board
      , gameId = gameId
      , token = token
      }
    , Cmd.none
    )


init : () -> ( Model, Cmd Msg )
init _ =
    initWithGameIdToken 1 "faa71988e5c4b226cd7607a7746e644a"


type Msg
    = GipfBoardMsg GipfBoard.Msg
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GipfBoardMsg gipfBoardMsg ->
            let
                ( newGipfBoard, gipfBoardCmd ) =
                    GipfBoard.update gipfBoardMsg model.board
            in
            ( { model | board = newGipfBoard }, Cmd.map GipfBoardMsg gipfBoardCmd )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.map GipfBoardMsg (GipfBoard.view model.board)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
