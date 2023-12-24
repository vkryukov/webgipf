module App exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Auth
import Browser
import Browser.Navigation as Nav
import Games
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Encode as Encode
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
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
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
    in
    -- TODO: Default gameType should be in sync with the select choices
    ( Model key url auth game
    , Cmd.batch
        [ Cmd.map AuthMsg authCmd
        , Cmd.map GamesMsg gameCmd
        ]
    )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | AuthMsg Auth.Msg
    | GamesMsg Games.Msg
    | NoOp


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

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )

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
