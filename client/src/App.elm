module App exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Auth
import Browser
import Browser.Navigation as Nav
import HomePage
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Encode as Encode
import PlayGame
import Routes
import Ui
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
    | SignInOrUp
    | PlayGame PlayGame.Model
    | ViewGame Int


type alias Model =
    { key : Nav.Key
    , page : Page
    , auth : Auth.Model
    , game : HomePage.Model
    }


init : Encode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( auth, authCmd ) =
            Auth.init flags

        ( game, gameCmd ) =
            HomePage.init auth.user

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
        , Cmd.map HomePageMsg gameCmd
        ]
    )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | NewRoute (Maybe Routes.Route)
    | AuthMsg Auth.Msg
    | HomePageMsg HomePage.Msg
    | PlayGameMsg PlayGame.Msg
    | NoOp


setNewPage : Maybe Routes.Route -> Model -> ( Model, Cmd Msg )
setNewPage maybeRoute model =
    case maybeRoute of
        Just Routes.Home ->
            if Auth.isAuthenticated model.auth then
                let
                    ( games, gamesCmd ) =
                        HomePage.updateModelWithUser model.auth.user model.game
                in
                ( { model | page = HomeSignedIn, game = games }, Cmd.map HomePageMsg gamesCmd )

            else
                ( { model | page = HomeSignedOut }, Cmd.none )

        Just Routes.SignIn ->
            let
                auth =
                    model.auth

                newAuth =
                    { auth | state = Auth.SigningIn }
            in
            ( { model | page = SignInOrUp, auth = newAuth }, Cmd.none )

        Just Routes.SignUp ->
            let
                auth =
                    model.auth

                newAuth =
                    { auth | state = Auth.SigningUp }
            in
            ( { model | page = SignInOrUp, auth = newAuth }, Cmd.none )

        Just Routes.SignOut ->
            let
                ( auth, authCmd ) =
                    Auth.signOut model.auth

                changeURL =
                    Routes.redirect model.key Routes.Home
            in
            ( { model | auth = auth }, Cmd.batch [ Cmd.map AuthMsg authCmd, changeURL ] )

        Just (Routes.ViewGame id) ->
            ( { model | page = ViewGame id }, Cmd.none )

        Just (Routes.PlayGame id) ->
            let
                ( playPageModel, playPageCmd ) =
                    PlayGame.initWithGameIdToken id model.auth.token
            in
            ( { model | page = PlayGame playPageModel }, Cmd.map PlayGameMsg playPageCmd )

        Nothing ->
            ( { model | page = NotFound }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.page, msg ) of
        ( _, LinkClicked urlRequest ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( _, NewRoute maybeRoute ) ->
            setNewPage maybeRoute model

        ( _, AuthMsg authMsg ) ->
            let
                ( auth, authCmd ) =
                    Auth.update authMsg model.auth

                changeURL =
                    if
                        Auth.isAuthenticated auth
                            && (model.page == HomeSignedOut || model.page == SignInOrUp)
                    then
                        Routes.redirect model.key Routes.Home

                    else
                        Cmd.none
            in
            ( { model
                | auth = auth
              }
            , Cmd.batch
                [ Cmd.map AuthMsg authCmd
                , changeURL
                ]
            )

        ( HomeSignedIn, HomePageMsg gamesMsg ) ->
            let
                ( games, gamesCmd ) =
                    HomePage.update gamesMsg model.game
            in
            ( { model | game = games }, Cmd.map HomePageMsg gamesCmd )

        ( PlayGame playGameModel, PlayGameMsg playGameMsg ) ->
            let
                ( newPlayGameModel, playGameCmd ) =
                    PlayGame.update playGameMsg playGameModel
            in
            ( { model | page = PlayGame newPlayGameModel }, Cmd.map PlayGameMsg playGameCmd )

        _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        PlayGame childModel ->
            Sub.map PlayGameMsg (PlayGame.subscriptions childModel)

        _ ->
            Sub.none



-- VIEW


viewSiteBar : Model -> String -> Html Msg
viewSiteBar model page =
    -- TODO: Display whether email is verified
    -- TODO: Add a button to resend verification email
    Ui.viewNavBar
        (case model.auth.user of
            Just user ->
                [ if page /= "" then
                    Ui.viewBreadcrumbs
                        [ ( Routes.Home, "Home" )
                        , ( Routes.Home, page )
                        ]

                  else
                    Ui.viewBoldText "Play GIPF"
                , Ui.viewText user.screenName
                , Ui.viewText ("<" ++ user.email ++ ">")
                ]

            Nothing ->
                [ Ui.viewBoldText "Play GIPF" ]
        )
        (case model.auth.user of
            Just _ ->
                [ Ui.viewLink "Sign out" (Routes.href Routes.SignOut) ]

            Nothing ->
                [ Ui.viewLink "Sign in" (Routes.href Routes.SignIn)
                , Ui.viewLink "Sign up" (Routes.href Routes.SignUp)
                ]
        )


view : Model -> Browser.Document Msg
view model =
    case model.page of
        HomeSignedOut ->
            { title = "Play Gipf"
            , body =
                [ viewSiteBar model ""
                , p [ class "p-4" ] [ text "Welcome to Play Gipf" ]
                ]
            }

        HomeSignedIn ->
            { title = "Home"
            , body =
                [ viewSiteBar model ""
                , Ui.viewSection "Create game"
                    [ Html.map HomePageMsg (HomePage.viewCreateNewGame model.game) ]
                , Ui.viewSection "Games you can join"
                    [ Html.map HomePageMsg (HomePage.viewJoinableGamesList model.game) ]
                , Ui.viewSection "Your games"
                    [ Html.map HomePageMsg (HomePage.viewOwnGamesList model.game)
                    ]
                ]
            }

        NotFound ->
            { title = "Page not found"
            , body =
                [ viewSiteBar model ""
                , p [] [ text "Page not found" ]
                ]
            }

        SignInOrUp ->
            { title =
                if model.auth.state == Auth.SigningIn then
                    "Signing in"

                else
                    "Singning up"
            , body =
                [ Html.map AuthMsg (Auth.view model.auth) ]
            }

        PlayGame playPageModel ->
            { title = "Play game"
            , body =
                [ viewSiteBar model ("Game " ++ String.fromInt playPageModel.gameId)
                , Html.map PlayGameMsg (PlayGame.view playPageModel)
                ]
            }

        _ ->
            { title = "Under construction", body = [ p [ class "p-4" ] [ text "Under construction" ] ] }
