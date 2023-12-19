module App exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Auth
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput)
import Json.Encode as Encode
import Ui exposing (viewPrimaryButton)
import Url



-- MAIN


main : Program Encode.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , auth : Auth.Model
    , gameType : String
    , color : String
    }


init : Encode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( auth, authCmd ) =
            Auth.init flags
    in
    -- TODO: Default gameType should be in sync with the select choices
    ( Model key url auth "Basic GIPF" "white", Cmd.map AuthMsg authCmd )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | AuthMsg Auth.Msg
    | SelectGameType String
    | SelectColor String
    | CreateGame
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )

        AuthMsg authMsg ->
            let
                ( auth, authCmd ) =
                    Auth.update authMsg model.auth
            in
            ( { model | auth = auth }, Cmd.map AuthMsg authCmd )

        SelectGameType gameType ->
            ( { model | gameType = gameType }, Cmd.none )

        SelectColor color ->
            ( { model | color = color }, Cmd.none )

        CreateGame ->
            let
                _ =
                    Debug.log "Create game" ( model.gameType, model.color )
            in
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Project Gipf"
    , body =
        [ Html.map AuthMsg (Auth.view model.auth)
        , viewCreateNewGame
        ]

    -- :: viewLinks model
    }


viewCreateNewGame : Html Msg
viewCreateNewGame =
    div [ class "p-4" ]
        [ h2 [ class "text-lg font-bold mb-4" ] [ text "Create new game" ]
        , div [ class "flex items-center space-x-4" ]
            [ label [ class "mr-2" ] [ text "Select game type:" ]
            , select [ class "form-select", onInput SelectGameType ]
                [ option [] [ text "Basic GIPF" ]
                , option [] [ text "Standard GIPF" ]
                , option [] [ text "Tournament GIPF" ]
                ]
            , label [ class "ml-4 mr-2" ] [ text "Play as:" ]
            , div [ class "flex items-center space-x-4" ]
                [ label []
                    [ input
                        [ type_ "radio"
                        , name "choice"
                        , value "white"
                        , class "form-radio"
                        , checked True
                        , onCheck
                            (\isChecked ->
                                if isChecked then
                                    SelectColor "white"

                                else
                                    NoOp
                            )
                        ]
                        []
                    , text " White"
                    ]
                , label []
                    [ input
                        [ type_ "radio"
                        , name "choice"
                        , value "black"
                        , class "form-radio"
                        , onCheck
                            (\isChecked ->
                                if isChecked then
                                    SelectColor "black"

                                else
                                    NoOp
                            )
                        ]
                        []
                    , text " Black"
                    ]
                ]
            , viewPrimaryButton ( "Create game", CreateGame )
            ]
        ]



-- viewLinks : Model -> List (Html msg)
-- viewLinks model =
--     [ text "The current URL is: "
--     , b [] [ text (Url.toString model.url) ]
--     , ul []
--         [ viewLink "/home"
--         , viewLink "/profile"
--         , viewLink "/reviews/the-century-of-the-self"
--         , viewLink "/reviews/public-opinion"
--         , viewLink "/reviews/shah-of-shahs"
--         ]
--     ]
-- viewLink : String -> Html msg
-- viewLink path =
--     li [] [ a [ href path ] [ text path ] ]
