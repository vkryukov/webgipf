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
    | Login
    | LoginReceived (Result Http.Error User)
    | Logout


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
    if token == "" then
        Cmd.none

    else
        Http.get
            { url = "http://localhost:8080/auth/check?token=" ++ token
            , expect = Http.expectJson LoginReceived userDecoder
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
    setStorage
        (Encode.object
            [ ( "token", Encode.string model.userStatus.token )
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


updateModelWithUserStatus : Result Http.Error User -> Model -> Model
updateModelWithUserStatus result model =
    case result of
        Ok user ->
            { model | user = Just user, userStatus = UserStatus user.token, error = Nothing }

        Err error ->
            { model | user = Nothing, userStatus = UserStatus "", error = Just (errorToString error) }


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

        Login ->
            ( model, login model )

        Logout ->
            let
                newModel =
                    { model | user = Nothing, userStatus = UserStatus "", error = Nothing }
            in
            ( newModel, savePreferences newModel )

        LoginReceived result ->
            let
                newModel =
                    updateModelWithUserStatus result model
            in
            ( newModel, savePreferences newModel )


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


viewUser : Model -> Html Msg
viewUser model =
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
                , button [ onClick Logout ] [ text "Logout" ]
                ]


view : Model -> Html Msg
view model =
    div []
        [ p [] [ text (Maybe.withDefault "" model.error) ]
        , viewUser model
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
