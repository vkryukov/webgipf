port module Auth exposing (..)

import Browser
import Html exposing (Html, button, div, input, p, text)
import Html.Attributes exposing (placeholder, type_)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Task
import Tools exposing (boolToString, errorToString)
import Ui exposing (Field, Form, viewForm)


type alias Model =
    { usernameInput : String
    , passwordInput : String
    , repeatPasswordInput : String
    , emailInput : String
    , user : Maybe User
    , userStatus : UserStatus
    , error : Maybe String
    , state : State
    }


type State
    = Initializing
    | SigningIn
    | SigningUp
    | SignedIn
    | UpdatingDetails


type Msg
    = UsernameInput String
    | PasswordInput String
    | RepeatPasswordInput String
    | EmailInput String
    | SignIn
    | ViewSignIn
    | SignUp
    | ViewSignUp
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
        Task.perform identity (Task.succeed ViewSignIn)

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


signUp : Model -> Cmd Msg
signUp model =
    let
        body =
            Http.jsonBody <|
                Encode.object
                    [ ( "username", Encode.string model.usernameInput )
                    , ( "password", Encode.string model.passwordInput )
                    , ( "email", Encode.string model.emailInput )
                    ]
    in
    Http.post
        { url = "http://localhost:8080/auth/register"
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
            ( Model "" "" "" "" Nothing status Nothing Initializing, checkUserStatus status.token )

        Err _ ->
            ( Model "" "" "" "" Nothing { token = "" } Nothing SigningIn
            , Cmd.none
            )


updateModelWithUserStatus : Result Http.Error User -> Model -> Model
updateModelWithUserStatus result model =
    -- TODO: updateModelWithUserStatus has a confusing name with userStatus that we save in preferences. Need to rename it.
    case result of
        Ok user ->
            { model | user = Just user, userStatus = UserStatus user.token, error = Nothing, state = SignedIn }

        Err error ->
            { model | user = Nothing, userStatus = UserStatus "", error = Just (errorToString error), state = SigningIn }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UsernameInput usernameInput ->
            ( { model | usernameInput = usernameInput }, Cmd.none )

        PasswordInput passwordInput ->
            ( { model | passwordInput = passwordInput }, Cmd.none )

        RepeatPasswordInput repeatPasswordInput ->
            ( { model | repeatPasswordInput = repeatPasswordInput }, Cmd.none )

        EmailInput emailInput ->
            ( { model | emailInput = emailInput }, Cmd.none )

        SignIn ->
            ( model, login model )

        ViewSignIn ->
            ( { model | state = SigningIn }, Cmd.none )

        SignUp ->
            ( model, signUp model )

        ViewSignUp ->
            ( { model | state = SigningUp }, Cmd.none )

        Logout ->
            let
                newModel =
                    { model | user = Nothing, userStatus = UserStatus "", error = Nothing, state = SigningIn }
            in
            ( newModel, savePreferences newModel )

        LoginReceived result ->
            let
                newModel =
                    updateModelWithUserStatus result model
            in
            ( newModel, savePreferences newModel )


viewSignIn : Model -> Html Msg
viewSignIn _ =
    let
        fields : List (Field Msg)
        fields =
            [ { label = "Username", fieldType = "text", placeholder = "Username", value = "", onInput = UsernameInput }
            , { label = "Password", fieldType = "password", placeholder = "Password", value = "", onInput = PasswordInput }
            ]

        form : Form Msg
        form =
            { title = "Sign In"
            , fields = fields
            , primaryAction = ( "Sign In", SignIn )
            , secondaryAction = ( "Sign Up", ViewSignUp )
            }
    in
    viewForm form


viewSignUp : Model -> Html Msg
viewSignUp _ =
    let
        fields : List (Field Msg)
        fields =
            [ { label = "Username", fieldType = "text", placeholder = "Username", value = "", onInput = UsernameInput }
            , { label = "Email", fieldType = "text", placeholder = "Email", value = "", onInput = EmailInput }
            , { label = "Password", fieldType = "password", placeholder = "Password", value = "", onInput = PasswordInput }
            , { label = "Repeat Password", fieldType = "password", placeholder = "Repeat Password", value = "", onInput = RepeatPasswordInput }
            ]

        form : Form Msg
        form =
            { title = "Sign Up"
            , fields = fields
            , primaryAction = ( "Sign Up", SignUp )
            , secondaryAction = ( "Sign In", ViewSignIn )
            }
    in
    viewForm form


viewUser : User -> Html Msg
viewUser user =
    div []
        [ div [] [ text ("Username: " ++ user.username) ]
        , div [] [ text ("Email: " ++ user.email) ]
        , div [] [ text ("Email Verified: " ++ boolToString user.emailVerified) ]
        , button [ onClick Logout ] [ text "Logout" ]
        ]


view : Model -> Html Msg
view model =
    case model.state of
        Initializing ->
            text "Initializing..."

        SigningIn ->
            viewSignIn model

        SigningUp ->
            viewSignUp model

        SignedIn ->
            viewUser (Maybe.withDefault { username = "", email = "", emailVerified = False, token = "" } model.user)

        UpdatingDetails ->
            text "Updating details..."


main : Program Encode.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
