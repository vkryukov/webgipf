module App exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Auth
import Browser
import Browser.Navigation as Nav
import Game
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Encode as Encode
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
    , game : Game.Model
    }


init : Encode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( auth, authCmd ) =
            Auth.init flags

        ( game, gameCmd ) =
            Game.init auth.user
    in
    -- TODO: Default gameType should be in sync with the select choices
    ( Model key url auth game
    , Cmd.batch
        [ Cmd.map AuthMsg authCmd
        , Cmd.map GameMsg gameCmd
        ]
    )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | AuthMsg Auth.Msg
    | GameMsg Game.Msg
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

                game =
                    Game.updateModelWithUser auth.user model.game
            in
            ( { model | auth = auth, game = game }, Cmd.map AuthMsg authCmd )

        GameMsg gameMsg ->
            let
                ( game, gameCmd ) =
                    Game.update gameMsg model.game
            in
            ( { model | game = game }, Cmd.map GameMsg gameCmd )

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
        , Html.map GameMsg (Game.viewCreateNewGame model.game)
        ]

    -- :: viewLinks model
    }



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
