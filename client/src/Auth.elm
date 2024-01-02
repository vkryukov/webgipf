port module Auth exposing (..)

import Html exposing (Html, div)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Routes
import ServerUtils exposing (HttpResult, parseResult, responseDecoder)
import Task
import Ui exposing (Field, Form, viewBoldText, viewForm, viewLink, viewNavBar, viewPrimaryButton, viewSiteTitle, viewText)



-- MODEL


type alias Model =
    { emailInput : String
    , screenNameInput : String
    , passwordInput : String
    , repeatPasswordInput : String
    , user : Maybe User
    , token : String
    , error : Maybe String
    , errorFields : List String
    , state : State
    }


type alias User =
    { email : String
    , emailVerified : Bool
    , screenName : String
    , token : String
    }


userDecoder : Decode.Decoder User
userDecoder =
    Decode.succeed User
        |> Pipeline.required "email" Decode.string
        |> Pipeline.required "email_verified" Decode.bool
        |> Pipeline.required "screen_name" Decode.string
        |> Pipeline.required "token" Decode.string


type State
    = Initializing
    | SigningIn
    | SigningUp
    | SignedIn


isAuthenticated : Model -> Bool
isAuthenticated model =
    model.state == SignedIn


initialModel : Model
initialModel =
    { emailInput = ""
    , screenNameInput = ""
    , passwordInput = ""
    , repeatPasswordInput = ""
    , user = Nothing
    , token = ""
    , error = Nothing
    , errorFields = []
    , state = Initializing
    }


type alias SavedModel =
    { token : String
    }


port setStorage : Encode.Value -> Cmd msg


save : Model -> Cmd msg
save model =
    setStorage
        (Encode.object
            [ ( "token", Encode.string model.token )
            ]
        )


savedModelDecoder : Decode.Decoder SavedModel
savedModelDecoder =
    Decode.map SavedModel
        (Decode.field "token" Decode.string)


init : Encode.Value -> ( Model, Cmd Msg )
init flags =
    case Decode.decodeValue savedModelDecoder flags of
        Ok savedModel ->
            ( { initialModel | token = savedModel.token }, checkUserStatus savedModel.token )

        Err _ ->
            ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = EmailInput String
    | PasswordInput String
    | RepeatPasswordInput String
    | ScreenNameInput String
    | SignIn
    | ViewSignIn
    | SignUp
    | ViewSignUp
    | LoginReceived (HttpResult User)


checkUserStatus : String -> Cmd Msg
checkUserStatus token =
    if token == "" then
        Task.perform identity (Task.succeed ViewSignIn)

    else
        Http.get
            { url = "/auth/check?token=" ++ token
            , expect = Http.expectJson LoginReceived (responseDecoder userDecoder)
            }


signIn : Model -> Cmd Msg
signIn model =
    let
        body =
            Http.jsonBody <|
                Encode.object
                    [ ( "email", Encode.string model.emailInput )
                    , ( "password", Encode.string model.passwordInput )
                    ]
    in
    Http.post
        { url = "/auth/signin"
        , body = body
        , expect = Http.expectJson LoginReceived (responseDecoder userDecoder)
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
        { url = "/auth/signup"
        , body = body
        , expect = Http.expectJson LoginReceived (responseDecoder userDecoder)
        }


updateModelWithUserResponse : HttpResult User -> Model -> Model
updateModelWithUserResponse result model =
    case parseResult result of
        Ok user ->
            { model
                | user = Just user
                , token = user.token
                , error = Nothing
                , errorFields = []
                , state = SignedIn
            }

        Err message ->
            { model
                | user = Nothing
                , token = ""
                , error = Just message

                -- TODO: set errorFields as well
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
                ( model, signIn model )

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

        LoginReceived result ->
            let
                newModel =
                    updateModelWithUserResponse result model
            in
            ( newModel, save newModel )


signOut : Model -> ( Model, Cmd Msg )
signOut model =
    let
        newModel =
            { model | user = Nothing, token = "", state = SigningIn }
    in
    ( newModel, save newModel )


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
            , actions =
                [ viewPrimaryButton ( "Sign In", SignIn )
                , viewLink "Sign Up" (Routes.href Routes.SignUp)
                ]
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
            , actions =
                [ viewPrimaryButton ( "Sign Up", SignUp )
                , viewLink "Sign In" (Routes.href Routes.SignIn)
                ]
            , error = model.error
            }
    in
    viewForm form


viewSiteBar : Model -> Html Msg
viewSiteBar model =
    -- TODO: Display whether email is verified
    -- TODO: Add a button to resend verification email
    viewNavBar
        (case model.user of
            Just user ->
                [ viewSiteTitle "Play GIPF"
                , viewBoldText user.screenName
                , viewText ("<" ++ user.email ++ ">")
                ]

            Nothing ->
                [ viewSiteTitle "Play GIPF" ]
        )
        (case model.user of
            Just _ ->
                [ viewLink "Sign out" (Routes.href Routes.SignOut) ]

            Nothing ->
                [ viewLink "Sign in" (Routes.href Routes.SignIn)
                , viewLink "Sign up" (Routes.href Routes.SignUp)
                ]
        )


view : Model -> Html Msg
view model =
    case model.state of
        SigningIn ->
            viewSignIn model

        SigningUp ->
            viewSignUp model

        _ ->
            div [] []
