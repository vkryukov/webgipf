module App exposing (Model, Msg(..), init, main, subscriptions, update, view, viewLink)

import Auth
import Browser
import Browser.Navigation as Nav
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
    }


init : Encode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( auth, authCmd ) =
            Auth.init flags
    in
    ( Model key url auth, Cmd.map AuthMsg authCmd )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | AuthMsg Auth.Msg


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Project Gipf"
    , body =
        Html.map AuthMsg (Auth.view model.auth)
            :: viewLinks model
    }


viewLinks : Model -> List (Html msg)
viewLinks model =
    [ text "The current URL is: "
    , b [] [ text (Url.toString model.url) ]
    , ul []
        [ viewLink "/home"
        , viewLink "/profile"
        , viewLink "/reviews/the-century-of-the-self"
        , viewLink "/reviews/public-opinion"
        , viewLink "/reviews/shah-of-shahs"
        ]
    ]


viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]
