port module Auth exposing (..)

import Browser
import Html exposing (Html, div, p, text)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Task
import Tools exposing (boolToString, errorToString)
import Ui exposing (Field, Form, viewForm, viewPrimaryButton)


type alias Model =
    { usernameInput : String
    , passwordInput : String
    , repeatPasswordInput : String
    , emailInput : String
    , user : Maybe User
    , userStatus : UserStatus
    , error : Maybe String
    , errorFields : List String
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
            ( Model "" "" "" "" Nothing status Nothing [] Initializing, checkUserStatus status.token )

        Err _ ->
            ( Model "" "" "" "" Nothing { token = "" } Nothing [] SigningIn
            , Cmd.none
            )


updateModelWithUserStatus : Result Http.Error User -> Model -> Model
updateModelWithUserStatus result model =
    -- TODO: updateModelWithUserStatus has a confusing name with userStatus that we save in preferences. Need to rename it.
    case result of
        Ok user ->
            { model | user = Just user, userStatus = UserStatus user.token, error = Nothing, state = SignedIn }

        Err error ->
            -- TODO: handle error properly, and if we were in sign up, we shouldn't switch to sign in.
            { model
                | user = Nothing
                , userStatus = UserStatus ""
                , error = Just (errorToString error)
                , state =
                    if model.state == Initializing then
                        SigningIn

                    else
                        model.state
            }


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
            if model.usernameInput == "" || model.passwordInput == "" then
                let
                    errorFields =
                        List.filter (\( _, input ) -> input == "")
                            [ ( "Username", model.usernameInput )
                            , ( "Password", model.passwordInput )
                            ]
                            |> List.map Tuple.first
                in
                ( { model | errorFields = errorFields, error = Just "All fields must be filled" }, Cmd.none )

            else
                ( model, login model )

        ViewSignIn ->
            ( { model | state = SigningIn, errorFields = [], error = Nothing }, Cmd.none )

        SignUp ->
            if model.usernameInput == "" || model.emailInput == "" || model.passwordInput == "" || model.repeatPasswordInput == "" then
                let
                    errorFields =
                        List.filter (\( _, input ) -> input == "")
                            [ ( "Username", model.usernameInput )
                            , ( "Email", model.emailInput )
                            , ( "Password", model.passwordInput )
                            , ( "Repeat Password", model.repeatPasswordInput )
                            ]
                            |> List.map Tuple.first
                in
                ( { model | errorFields = errorFields, error = Just "All fields must be filled" }, Cmd.none )

            else if model.passwordInput /= model.repeatPasswordInput then
                let
                    errorFields =
                        [ "Password", "Repeat Password" ]
                in
                ( { model | errorFields = errorFields, error = Just "Passwords must match" }, Cmd.none )

            else
                ( model, signUp model )

        ViewSignUp ->
            ( { model | state = SigningUp, errorFields = [], error = Nothing }, Cmd.none )

        Logout ->
            let
                newModel =
                    { model
                        | usernameInput = ""
                        , passwordInput = ""
                        , repeatPasswordInput = ""
                        , emailInput = ""
                        , user = Nothing
                        , userStatus = UserStatus ""
                        , error = Nothing
                        , state = SigningIn
                    }
            in
            ( newModel, savePreferences newModel )

        LoginReceived result ->
            let
                newModel =
                    updateModelWithUserStatus result model
            in
            ( newModel, savePreferences newModel )


highlightErrorFields : List String -> List (Field msg) -> List (Field msg)
highlightErrorFields errorFields fields =
    List.map
        (\field ->
            if List.member field.label errorFields then
                { field | highlight = True }

            else
                field
        )
        fields


viewSignIn : Model -> Html Msg
viewSignIn model =
    let
        fields : List (Field Msg)
        fields =
            [ { label = "Username", fieldType = "text", placeholder = "Username", value = model.usernameInput, onInput = UsernameInput, highlight = False }
            , { label = "Password", fieldType = "password", placeholder = "Password", value = model.passwordInput, onInput = PasswordInput, highlight = False }
            ]

        form : Form Msg
        form =
            { title = "Sign In"
            , fields = highlightErrorFields model.errorFields fields
            , primaryAction = ( "Sign In", SignIn )
            , secondaryAction = ( "Sign Up", ViewSignUp )
            , error = model.error
            }
    in
    viewForm form


viewSignUp : Model -> Html Msg
viewSignUp model =
    let
        fields : List (Field Msg)
        fields =
            [ { label = "Username", fieldType = "text", placeholder = "Username", value = model.usernameInput, onInput = UsernameInput, highlight = False }
            , { label = "Email", fieldType = "text", placeholder = "Email", value = model.emailInput, onInput = EmailInput, highlight = False }
            , { label = "Password", fieldType = "password", placeholder = "Password", value = model.passwordInput, onInput = PasswordInput, highlight = False }
            , { label = "Repeat Password", fieldType = "password", placeholder = "Repeat Password", value = model.repeatPasswordInput, onInput = RepeatPasswordInput, highlight = False }
            ]

        form : Form Msg
        form =
            { title = "Sign Up"
            , fields = highlightErrorFields model.errorFields fields
            , primaryAction = ( "Sign Up", SignUp )
            , secondaryAction = ( "Sign In", ViewSignIn )
            , error = model.error
            }
    in
    viewForm form


viewUser : User -> Html Msg
viewUser user =
    div []
        [ div [] [ text ("Username: " ++ user.username) ]
        , div [] [ text ("Email: " ++ user.email) ]
        , div [] [ text ("Email Verified: " ++ boolToString user.emailVerified) ]
        , viewPrimaryButton ( "Logout", Logout )
        ]


view : Model -> Html Msg
view model =
    div []
        [ case model.state of
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
        ]


main : Program Encode.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
