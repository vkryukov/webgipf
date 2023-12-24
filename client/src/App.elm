module App exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Auth
import Browser
import Browser.Navigation as Nav
import Games
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Encode as Encode
import Routes
import Ui exposing (viewSection)
import Url
import Url.Parser exposing ((</>))



-- MAIN


main : Program Encode.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = Routes.match >> NewRoute
        }



-- MODEL


type Page
    = NotFound
    | HomeSignedIn
    | HomeSignedOut
    | SignIn
    | PlayGame Int
    | ViewGame Int


type alias Model =
    { key : Nav.Key
    , page : Page
    , auth : Auth.Model
    , game : Games.Model
    }


init : Encode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( auth, authCmd ) =
            Auth.init flags

        ( game, gameCmd ) =
            Games.init auth.user

        initialModel =
            Model key NotFound auth game

        ( model, cmd ) =
            setNewPage (Routes.match url) initialModel
    in
    -- TODO: Default gameType should be in sync with the select choices
    ( model
    , Cmd.batch
        [ cmd
        , Cmd.map AuthMsg authCmd
        , Cmd.map GamesMsg gameCmd
        ]
    )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | NewRoute (Maybe Routes.Route)
    | AuthMsg Auth.Msg
    | GamesMsg Games.Msg
    | NoOp


setNewPage : Maybe Routes.Route -> Model -> ( Model, Cmd Msg )
setNewPage maybeRoute model =
    case maybeRoute of
        Just Routes.Home ->
            if Auth.isAuthenticated model.auth then
                ( { model | page = HomeSignedIn }, Cmd.none )

            else
                ( { model | page = HomeSignedOut }, Cmd.none )

        Just Routes.SignIn ->
            ( { model | page = SignIn }, Cmd.none )

        Just (Routes.ViewGame id) ->
            ( { model | page = ViewGame id }, Cmd.none )

        Just (Routes.PlayGame id) ->
            ( { model | page = PlayGame id }, Cmd.none )

        Nothing ->
            ( { model | page = NotFound }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    let
                        parser =
                            Url.Parser.s "game" </> Url.Parser.s "cancel" </> Url.Parser.int

                        maybeId =
                            Url.Parser.parse parser url
                    in
                    case maybeId of
                        Just id ->
                            let
                                gamesCmd =
                                    Games.cancelGame id model.game
                            in
                            ( model
                            , Cmd.batch
                                [ Cmd.map GamesMsg gamesCmd
                                ]
                            )

                        Nothing ->
                            ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        NewRoute maybeRoute ->
            setNewPage maybeRoute model

        AuthMsg authMsg ->
            let
                ( auth, authCmd ) =
                    Auth.update authMsg model.auth

                ( games, gamesCmd ) =
                    -- TODO: make sure that the logout is also handled
                    Games.updateModelWithUser auth.user model.game
            in
            ( { model | auth = auth, game = games }
            , Cmd.batch
                [ Cmd.map AuthMsg authCmd
                , Cmd.map GamesMsg gamesCmd
                ]
            )

        GamesMsg gamesMsg ->
            let
                ( games, gamesCmd ) =
                    Games.update gamesMsg model.game
            in
            ( { model | game = games }, Cmd.map GamesMsg gamesCmd )

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
        , Html.map GamesMsg (Games.viewCreateNewGame model.game)
        , viewSection "Joinable games"
            [ Html.map GamesMsg (Games.viewJoinableGamesList model.game) ]
        , viewSection "Your games"
            [ Html.map GamesMsg (Games.viewOwnGamesList model.game) ]
        ]
    }
