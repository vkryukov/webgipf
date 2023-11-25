module GipfViewer exposing (..)

import Browser
import GipfBoard
import Html exposing (..)
import Html.Attributes exposing (disabled, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter, onMouseLeave)


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
    , hoverOverAction : Maybe Int
    , boardInput : String
    }


initFromString : String -> ( Model, Cmd Msg )
initFromString str =
    ( { actions = String.split " " str
      , currentAction = 0
      , hoverOverAction = Nothing
      , boardInput = ""
      }
    , Cmd.none
    )


init : () -> ( Model, Cmd Msg )
init _ =
    initFromString "GWi3-h3 GKb6-c6 GWi2-h3 GKc7-c6 GWi2-h3 GKc7-c6 Wi4-h4 Ka5-b5 Wi3-h4 Ki4-h4 Wi3-h4 Kg7-g6 Wb6-c6 Kb6-c6 Wi3-h4 Kf8-f7 Wg1-f2 Ka5-b5 xf6,g6 Wa4-b5 Ka4-b5 Wd8-d7 Ke9-e8 xc5,d6,e7,f7 xe6,g4 Wb6-c6 Ka5-b5 Wh6-h5 Ki4-h5 Wi4-h5 Kg7-g6 Wi3-h4 Kh6-h5 Wa5-b5 Ki3-h3 Wi4-h4"



-- Update


type Msg
    = SaveBoardInput String
    | UpdateBoard
    | Click Int
    | MouseEnter Int
    | MouseLeave
    | GoToStart
    | GoBack
    | GoForward
    | GoToEnd
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

        MouseEnter action ->
            ( { model
                | hoverOverAction =
                    if action /= model.currentAction then
                        Just action

                    else
                        Nothing
              }
            , Cmd.none
            )

        MouseLeave ->
            ( { model | hoverOverAction = Nothing }, Cmd.none )

        GoToStart ->
            ( { model | currentAction = 0 }, Cmd.none )

        GoBack ->
            ( { model | currentAction = max 0 (model.currentAction - 1) }, Cmd.none )

        GoForward ->
            ( { model | currentAction = min (List.length model.actions - 1) (model.currentAction + 1) }, Cmd.none )

        GoToEnd ->
            ( { model | currentAction = List.length model.actions - 1 }, Cmd.none )

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
    { m | allowActions = False, showDebug = False }


viewAction : Int -> Maybe Int -> Int -> String -> Html Msg
viewAction current hover i a =
    div
        ([ style "display" "inline-block"
         , style "padding-left" "5px"
         , style "cursor" "pointer"
         , onClick (Click i)
         , onMouseEnter (MouseEnter i)
         , onMouseLeave MouseLeave
         ]
            ++ (if i == current then
                    [ style "background-color" "#ffff99" ]

                else
                    []
               )
            ++ (if i == Maybe.withDefault -1 hover then
                    [ style "background-color" "#ffffdd" ]

                else
                    []
               )
        )
        [ text a ]


view : Model -> Html Msg
view model =
    div
        [ style "padding-left" "25px"
        , style "padding-top" "10px"
        ]
        [ div []
            [ Html.map BoardMsg (GipfBoard.view (board model))
            , div [ style "padding-top" "10px" ]
                [ button [ onClick GoToStart ] [ text "Start" ]
                , div [ style "width" "10px", style "display" "inline-block" ] []
                , button [ onClick GoBack ] [ text "<" ]
                , div [ style "width" "10px", style "display" "inline-block" ] []
                , button [ onClick GoForward ] [ text ">" ]
                , div [ style "width" "10px", style "display" "inline-block" ] []
                , button [ onClick GoToEnd ] [ text "End" ]
                ]
            , div
                [ style "padding-top" "10px"
                , style "width" "610px"
                ]
                (List.indexedMap
                    (viewAction model.currentAction model.hoverOverAction)
                    model.actions
                )
            , div
                [ style "width" "610px"
                , style "padding-top" "10px"
                ]
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
        ]
