module ServerAdmin exposing (..)

import Browser
import Fuzz exposing (result)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (placeholder, style, type_)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Json.Encode as Encode


{-|

    This server provides UX to assist with server management.

-}
main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }


serverURL : String
serverURL =
    "http://localhost:8080"


listUsersURL : String
listUsersURL =
    serverURL ++ "/users"


registerUserURL : String
registerUserURL =
    serverURL ++ "/register"


type alias Model =
    { username : String
    , password : String
    , token : String
    , error : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { username = ""
      , password = ""
      , token = ""
      , error = ""
      }
    , Cmd.none
    )


type Msg
    = UpdateUsername String
    | UpdatePassword String
    | Register
    | RegisterResult (Result Http.Error String)


tokenDecoder : Decode.Decoder String
tokenDecoder =
    Decode.field "token" Decode.string


registerRequest : Model -> Cmd Msg
registerRequest model =
    let
        body =
            Encode.object
                [ ( "username", Encode.string model.username )
                , ( "password", Encode.string model.password )
                ]
    in
    Http.post
        { url = registerUserURL
        , body = Http.jsonBody body
        , expect = Http.expectJson RegisterResult tokenDecoder
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateUsername username ->
            ( { model | username = username }, Cmd.none )

        UpdatePassword password ->
            ( { model | password = password }, Cmd.none )

        Register ->
            -- Here you would send a request to the server to register the user
            ( model, registerRequest model )

        RegisterResult result ->
            case result of
                Ok token ->
                    -- Registration was successful, update the token
                    ( { model | token = token }, Cmd.none )

                Err _ ->
                    -- An error occurred, update the error message
                    ( { model | error = "Registration failed" }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        tokenDiv =
            if model.token /= "" then
                [ div [] [ text ("Token: " ++ model.token) ] ]

            else
                []

        errorDiv =
            if model.error /= "" then
                [ div [] [ text ("Error: " ++ model.error) ] ]

            else
                []
    in
    div [ style "margin" "10px" ]
        ([ input [ placeholder "Username", type_ "text", onInput UpdateUsername ] []
         , input [ placeholder "Password", type_ "password", onInput UpdatePassword ] []
         , button [ onClick Register ] [ text "Register" ]
         ]
            ++ tokenDiv
            ++ errorDiv
        )
