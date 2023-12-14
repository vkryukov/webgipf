port module Auth exposing (..)

import Browser
import Html exposing (Html, button, div, input, p, text)
import Html.Attributes exposing (placeholder, type_)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Json.Encode as Encode


type alias Model =
    { usernameInput : String
    , passwordInput : String
    , newPasswordInput : String
    , emailInput : String
    , user : Maybe User
    , userStatus : UserStatus
    , error : Maybe String
    }


type Msg
    = UsernameInput String
    | PasswordInput String
    | NewPasswordInput String
    | EmailInput String
    | UserStatusReceived (Result Http.Error User)
    | Login
    | LoginReceived (Result Http.Error User)


type alias User =
    { username : String
    , email : String
    , emailVerified : Bool
    , token : String
    }


userDecoder : Decode.Decoder User
userDecoder =
    Decode.map4 User
        (Decode.field "username" Decode.string)
        (Decode.field "email" Decode.string)
        (Decode.field "email_verified" Decode.bool)
        (Decode.field "token" Decode.string)


type alias UserStatus =
    { token : String
    }


userStatusDecoder : Decode.Decoder UserStatus
userStatusDecoder =
    Decode.map UserStatus
        (Decode.field "token" Decode.string)


checkUserStatus : String -> Cmd Msg
checkUserStatus token =
    Http.get
        { url = "http://localhost:8080/auth/check?token=" ++ token
        , expect = Http.expectJson UserStatusReceived userDecoder
        }


login : Model -> Cmd Msg
login model =
    let
        body =
            Http.jsonBody <|
                Encode.object
                    [ ( "username", Encode.string model.usernameInput )
                    , ( "password", Encode.string model.passwordInput )
                    ]
    in
    Http.post
        { url = "http://localhost:8080/auth/login"
        , body = body
        , expect = Http.expectJson LoginReceived userDecoder
        }


port setStorage : Encode.Value -> Cmd msg


savePreferences : Model -> Cmd msg
savePreferences model =
    case model.user of
        Nothing ->
            Cmd.none

        Just user ->
            setStorage
                (Encode.object
                    [ ( "token", Encode.string user.token )
                    ]
                )


init : Encode.Value -> ( Model, Cmd Msg )
init flags =
    case Decode.decodeValue userStatusDecoder flags of
        Ok status ->
            ( Model "" "" "" "" Nothing status Nothing, checkUserStatus status.token )

        Err _ ->
            ( Model "" "" "" "" Nothing { token = "" } Nothing
            , Cmd.none
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UsernameInput usernameInput ->
            ( { model | usernameInput = usernameInput }, Cmd.none )

        PasswordInput passwordInput ->
            ( { model | passwordInput = passwordInput }, Cmd.none )

        NewPasswordInput newPasswordInput ->
            ( { model | newPasswordInput = newPasswordInput }, Cmd.none )

        EmailInput emailInput ->
            ( { model | emailInput = emailInput }, Cmd.none )

        UserStatusReceived (Ok user) ->
            ( { model | user = Just user, error = Nothing }, Cmd.none )

        UserStatusReceived (Err error) ->
            ( { model | error = Just (errorToString error) }, Cmd.none )

        Login ->
            ( model, login model )

        LoginReceived (Ok user) ->
            let
                newModel =
                    { model | user = Just user, error = Nothing }
            in
            ( newModel, savePreferences newModel )

        LoginReceived (Err error) ->
            ( { model | error = Just (errorToString error) }, Cmd.none )


errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Request timed out"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus statusCode ->
            "Bad status: " ++ String.fromInt statusCode

        Http.BadBody message ->
            "Bad body: " ++ message


view : Model -> Html Msg
view model =
    case model.user of
        Nothing ->
            div []
                [ p [] [ text (Maybe.withDefault "" model.error) ]
                , p []
                    [ text "User is not logged in" ]
                , input
                    [ type_ "text", placeholder "Username", onInput UsernameInput ]
                    []
                , input [ type_ "password", placeholder "Password", onInput PasswordInput ] []
                , button [ onClick Login ] [ text "Login" ]
                ]

        Just user ->
            div []
                [ div [] [ text ("Username: " ++ user.username) ]
                , div [] [ text ("Email: " ++ user.email) ]
                , div [] [ text ("Email Verified: " ++ boolToString user.emailVerified) ]
                ]


boolToString : Bool -> String
boolToString bool =
    if bool then
        "True"

    else
        "False"


main : Program Encode.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
