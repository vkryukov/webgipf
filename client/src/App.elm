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
    | SignUp
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
            let
                auth =
                    model.auth

                newAuth =
                    { auth | state = Auth.SigningIn }
            in
            ( { model | page = SignIn, auth = newAuth }, Cmd.none )

        Just Routes.SignUp ->
            let
                auth =
                    model.auth

                newAuth =
                    { auth | state = Auth.SigningUp }
            in
            ( { model | page = SignUp, auth = newAuth }, Cmd.none )

        Just Routes.SignOut ->
            let
                ( auth, authCmd ) =
                    Auth.signOut model.auth
            in
            ( { model | page = HomeSignedOut, auth = auth }, Cmd.map AuthMsg authCmd )

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
            ( { model
                | auth = auth
                , game = games
                , page =
                    if Auth.isAuthenticated auth && model.page == HomeSignedOut then
                        HomeSignedIn

                    else
                        HomeSignedOut
              }
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


viewSiteBar : Model -> Html Msg
viewSiteBar model =
    Html.map AuthMsg (Auth.viewSiteBar model.auth)


view : Model -> Browser.Document Msg
view model =
    case model.page of
        HomeSignedOut ->
            { title = "Project Gipf"
            , body =
                [ viewSiteBar model
                , p [ class "p-4" ] [ text "Welcome to Project Gipf" ]
                ]
            }

        HomeSignedIn ->
            { title = "Home"
            , body =
                [ viewSiteBar model
                , viewSection "Joinable games"
                    [ Html.map GamesMsg (Games.viewJoinableGamesList model.game) ]
                , viewSection "Your games"
                    [ Html.map GamesMsg (Games.viewOwnGamesList model.game)
                    ]
                ]
            }

        NotFound ->
            { title = "Page not found"
            , body =
                [ viewSiteBar model
                , p [] [ text "Page not found" ]
                ]
            }

        SignIn ->
            { title = "Signing in"
            , body =
                [ Html.map AuthMsg (Auth.view model.auth) ]
            }

        SignUp ->
            { title = "Signing up"
            , body =
                [ Html.map AuthMsg (Auth.view model.auth) ]
            }

        _ ->
            { title = "Under constructoin", body = [ p [ class "p-4" ] [ text "Under construction" ] ] }
