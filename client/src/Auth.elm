port module Auth exposing (..)

import Browser
import Html exposing (Html, div, p, text)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Task
import Tools exposing (errorToString)
import Ui exposing (Field, Form, viewBoldText, viewForm, viewNavBar, viewText)


type alias Model =
    { emailInput : String
    , screenNameInput : String
    , passwordInput : String
    , repeatPasswordInput : String
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
    = EmailInput String
    | PasswordInput String
    | RepeatPasswordInput String
    | ScreenNameInput String
    | SignIn
    | ViewSignIn
    | SignUp
    | ViewSignUp
    | LoginReceived (Result Http.Error UserResponse)
    | Logout


type alias User =
    { email : String
    , emailVerified : Bool
    , screenName : String
    , token : String
    }


userDecoder : Decode.Decoder User
userDecoder =
    Decode.map4 User
        (Decode.field "email" Decode.string)
        (Decode.field "email_verified" Decode.bool)
        (Decode.field "screen_name" Decode.string)
        (Decode.field "token" Decode.string)


type alias UserStatus =
    { token : String
    }


userStatusDecoder : Decode.Decoder UserStatus
userStatusDecoder =
    Decode.map UserStatus
        (Decode.field "token" Decode.string)


type UserResponse
    = UserResponse User
    | ErrorResponse String


userResponseDecoder : Decode.Decoder UserResponse
userResponseDecoder =
    Decode.oneOf
        [ Decode.map UserResponse userDecoder
        , Decode.map ErrorResponse (Decode.field "error" Decode.string)
        ]


checkUserStatus : String -> Cmd Msg
checkUserStatus token =
    if token == "" then
        Task.perform identity (Task.succeed ViewSignIn)

    else
        Http.get
            { url = "http://localhost:8080/auth/check?token=" ++ token
            , expect = Http.expectJson LoginReceived userResponseDecoder
            }


login : Model -> Cmd Msg
login model =
    let
        body =
            Http.jsonBody <|
                Encode.object
                    [ ( "email", Encode.string model.emailInput )
                    , ( "password", Encode.string model.passwordInput )
                    ]
    in
    Http.post
        { url = "http://localhost:8080/auth/login"
        , body = body
        , expect = Http.expectJson LoginReceived userResponseDecoder
        }


signUp : Model -> Cmd Msg
signUp model =
    let
        body =
            Http.jsonBody <|
                Encode.object
                    [ ( "email", Encode.string model.emailInput )
                    , ( "password", Encode.string model.passwordInput )
                    , ( "screen_name", Encode.string model.screenNameInput )
                    ]
    in
    Http.post
        { url = "http://localhost:8080/auth/register"
        , body = body
        , expect = Http.expectJson LoginReceived userResponseDecoder
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


updateModelWithUserResponse : Result Http.Error UserResponse -> Model -> Model
updateModelWithUserResponse result model =
    case result of
        Ok (UserResponse user) ->
            { model | user = Just user, userStatus = UserStatus user.token, error = Nothing, state = SignedIn }

        Ok (ErrorResponse error) ->
            { model
                | user = Nothing
                , userStatus = UserStatus ""
                , error = Just error
                , state =
                    if model.state == Initializing then
                        SigningIn

                    else
                        model.state
            }

        Err error ->
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
        EmailInput emailInput ->
            ( { model | emailInput = emailInput }, Cmd.none )

        PasswordInput passwordInput ->
            ( { model | passwordInput = passwordInput }, Cmd.none )

        RepeatPasswordInput repeatPasswordInput ->
            ( { model | repeatPasswordInput = repeatPasswordInput }, Cmd.none )

        ScreenNameInput screenNameInput ->
            ( { model | screenNameInput = screenNameInput }, Cmd.none )

        SignIn ->
            if model.emailInput == "" || model.passwordInput == "" then
                let
                    errorFields =
                        List.filter (\( _, input ) -> input == "")
                            [ ( "Email", model.emailInput )
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
            if model.emailInput == "" || model.passwordInput == "" || model.repeatPasswordInput == "" then
                let
                    errorFields =
                        List.filter (\( _, input ) -> input == "")
                            [ ( "Username", model.emailInput )
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
                        | emailInput = ""
                        , passwordInput = ""
                        , repeatPasswordInput = ""
                        , screenNameInput = ""
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
                    updateModelWithUserResponse result model
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
            [ { label = "Email", fieldType = "text", placeholder = "Email", value = model.emailInput, onInput = EmailInput, highlight = False }
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
            [ { label = "Email", fieldType = "text", placeholder = "Email", value = model.emailInput, onInput = EmailInput, highlight = False }
            , { label = "Screen Name", fieldType = "text", placeholder = "Screen Name", value = model.screenNameInput, onInput = ScreenNameInput, highlight = False }
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
    -- TODO: Display whether email is verified
    -- TODO: Add a button to resend verification email
    let
        items =
            [ viewBoldText user.screenName
            , viewText ("<" ++ user.email ++ ">")
            ]

        actions =
            [ ( "Sign out", Logout ) ]
    in
    viewNavBar items actions


view : Model -> Html Msg
view model =
    div []
        [ case model.state of
            Initializing ->
                text "Connecting..."

            SigningIn ->
                viewSignIn model

            SigningUp ->
                viewSignUp model

            SignedIn ->
                viewUser (Maybe.withDefault { email = "", emailVerified = False, screenName = "", token = "" } model.user)

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
