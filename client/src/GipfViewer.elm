module GipfViewer exposing (..)

import Browser
import GipfBoard
import Html exposing (..)
import Html.Attributes exposing (disabled, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }



-- Model


type alias Model =
    { actions : List String
    , currentAction : Int
    , boardInput : String
    }


initFromString : String -> ( Model, Cmd Msg )
initFromString str =
    ( { actions = String.split " " str
      , currentAction = 0
      , boardInput = ""
      }
    , Cmd.none
    )


init : () -> ( Model, Cmd Msg )
init _ =
    initFromString ""



-- Update


type Msg
    = SaveBoardInput String
    | UpdateBoard
    | Click Int
    | BoardMsg GipfBoard.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SaveBoardInput str ->
            ( { model | boardInput = str }, Cmd.none )

        UpdateBoard ->
            let
                ( m, cmd ) =
                    initFromString model.boardInput
            in
            ( { m | boardInput = "" }, cmd )

        Click action ->
            ( { model | currentAction = action }, Cmd.none )

        BoardMsg _ ->
            ( model, Cmd.none )



-- View


board : Model -> GipfBoard.Model
board model =
    let
        str =
            String.join " " (List.take (model.currentAction + 1) model.actions)

        ( m, _ ) =
            GipfBoard.initFromString str
    in
    m


view : Model -> Html Msg
view model =
    div
        [ style "padding-left" "25px"
        , style "padding-top" "10px"
        ]
        [ div []
            [ Html.map BoardMsg (GipfBoard.view (board model))
            ]
        , div [ style "width" "610px" ]
            [ input
                [ onInput SaveBoardInput
                , style "width" "530px"
                , type_ "text"
                , placeholder "Enter new game..."
                , value model.boardInput
                ]
                []
            , div [ style "width" "10px", style "display" "inline-block" ] []
            , button [ onClick UpdateBoard, disabled (String.isEmpty model.boardInput) ] [ text "Update" ]
            ]
        ]
