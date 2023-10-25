module Board exposing (..)

import Browser
import Html exposing (Html, div, text)
import String



-- Model definition


type alias Model =
    Int


init : Model
init =
    0



-- Update


type Msg
    = NoOp


update : Msg -> Model -> Model
update _ model =
    model



-- View


view : Model -> Html Msg
view model =
    div [] [ text (String.fromInt model) ]


main =
    Browser.sandbox { init = init, update = update, view = view }
