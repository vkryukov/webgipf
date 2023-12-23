module Games exposing
    ( Model
    , Msg(..)
    , init
    , update
    , updateModelWithUser
    , viewCreateNewGame
    , viewJoinableGamesList
    , viewOwnGamesList
    )

import Auth
import Html exposing (Html, a, div, label, option, select, text)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onInput)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import ServerUtils exposing (HttpResult, parseResult, responseDecoder)
import Time exposing (Month(..))
import Ui exposing (viewErrorMessage, viewHtmlTable, viewPrimaryButton, viewRadio, viewSection)


type alias Model =
    { screenName : String
    , token : String
    , gameType : String
    , color : String
    , error : Maybe String
    , ownGames : List Game
    , joinableGames : List Game
    }


init : Maybe Auth.User -> ( Model, Cmd Msg )
init maybeUser =
    updateModelWithUser maybeUser
        { screenName = ""
        , token = ""
        , gameType = "Basic GIPF"
        , color = "white"
        , error = Nothing
        , ownGames = []
        , joinableGames = []
        }


updateModelWithUser : Maybe Auth.User -> Model -> ( Model, Cmd Msg )
updateModelWithUser maybeUser model =
    case maybeUser of
        Just user ->
            let
                newModel =
                    { model | screenName = user.screenName, token = user.token }
            in
            ( newModel, Cmd.batch [ ownGames newModel, joinableGames newModel ] )

        Nothing ->
            ( { model | screenName = "", token = "", ownGames = [], joinableGames = [] }, Cmd.none )


type Msg
    = SelectGameType String
    | SelectColor String
    | CreateGame
    | CreateGameReceived (HttpResult Game)
    | OwnGamesReceived (HttpResult (List Game))
    | JoinableGamesReceived (HttpResult (List Game))
    | JoinGame Int
    | JoinedGameReceved (HttpResult Game)
    | NoOp


type alias Game =
    { id : Int
    , gameType : String
    , whitePlayer : String
    , blackPlayer : String
    , whiteToken : String
    , blackToken : String
    , numActions : Int
    }


gameDecoder : Decode.Decoder Game
gameDecoder =
    Decode.map7 Game
        (Decode.field "id" Decode.int)
        (Decode.field "type" Decode.string)
        (Decode.field "white_player" Decode.string)
        (Decode.field "black_player" Decode.string)
        (Decode.field "white_token" Decode.string)
        (Decode.field "black_token" Decode.string)
        (Decode.field "num_actions" Decode.int)


gameListDecoder : Decode.Decoder (List Game)
gameListDecoder =
    Decode.list gameDecoder


createGame : Model -> Cmd Msg
createGame model =
    Http.post
        { url = "/game/create"
        , body =
            Http.jsonBody <|
                Encode.object
                    [ ( "type", Encode.string model.gameType )
                    , ( model.color ++ "_player", Encode.string model.screenName )
                    , ( model.color ++ "_token", Encode.string model.token )
                    ]
        , expect = Http.expectJson CreateGameReceived (responseDecoder gameDecoder)
        }


getGames : String -> (HttpResult (List Game) -> Msg) -> Model -> Cmd Msg
getGames url cmd model =
    if model.token == "" then
        Cmd.none

    else
        Http.post
            { url = url
            , body =
                Http.jsonBody <|
                    Encode.object
                        [ ( "token", Encode.string model.token ) ]
            , expect = Http.expectJson cmd (responseDecoder gameListDecoder)
            }


ownGames : Model -> Cmd Msg
ownGames model =
    getGames "/game/list/byuser" OwnGamesReceived model


joinableGames : Model -> Cmd Msg
joinableGames model =
    getGames "/game/list/joinable" JoinableGamesReceived model


joinGame : Int -> Model -> Cmd Msg
joinGame gameId model =
    Http.post
        { url = "/game/join"
        , body =
            Http.jsonBody <|
                Encode.object
                    [ ( "token", Encode.string model.token )
                    , ( "id", Encode.int gameId )
                    ]
        , expect = Http.expectJson JoinedGameReceved (responseDecoder gameDecoder)
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectGameType gameType ->
            ( { model | gameType = gameType }, Cmd.none )

        SelectColor color ->
            ( { model | color = color }, Cmd.none )

        CreateGame ->
            ( model, createGame model )

        CreateGameReceived result ->
            case parseResult result of
                Ok _ ->
                    ( { model | error = Nothing }, ownGames model )

                Err error ->
                    ( { model | error = Just error }, Cmd.none )

        OwnGamesReceived result ->
            case parseResult result of
                Ok games ->
                    ( { model | ownGames = games }, Cmd.none )

                Err error ->
                    ( { model | error = Just error }, Cmd.none )

        JoinableGamesReceived result ->
            case parseResult result of
                Ok games ->
                    ( { model | joinableGames = games }, Cmd.none )

                Err error ->
                    ( { model | error = Just error }, Cmd.none )

        JoinGame gameId ->
            ( model, joinGame gameId model )

        JoinedGameReceved result ->
            case parseResult result of
                Ok _ ->
                    ( { model | error = Nothing }, Cmd.batch [ ownGames model, joinableGames model ] )

                Err error ->
                    ( { model | error = Just error }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


viewCreateNewGame : Model -> Html Msg
viewCreateNewGame model =
    if model.screenName == "" then
        div [] []

    else
        viewSection "Create new game"
            [ div [ class "flex items-center space-x-4" ]
                [ label [ class "mr-2" ] [ text "Select game type:" ]
                , select [ class "form-select", onInput SelectGameType ]
                    [ option [] [ text "Basic GIPF" ]
                    , option [] [ text "Standard GIPF" ]
                    , option [] [ text "Tournament GIPF" ]
                    ]
                , label [ class "ml-4 mr-2" ] [ text "Play as:" ]
                , div [ class "flex items-center space-x-4" ]
                    [ viewRadio (model.color == "white") "White" (SelectColor "white") NoOp
                    , viewRadio (model.color == "black") "Black" (SelectColor "black") NoOp
                    ]
                , viewPrimaryButton ( "Create game", CreateGame )
                ]
            , viewErrorMessage model.error
            ]


viewOwnGamesList : Model -> Html Msg
viewOwnGamesList model =
    viewHtmlTable model.ownGames
        [ "Game Id", "Game type", "White Player", "Black Player", "Num Actions" ]
        [ \game -> text (String.fromInt game.id)
        , \game -> text game.gameType
        , \game -> text game.whitePlayer
        , \game -> text game.blackPlayer
        , \game -> text (String.fromInt game.numActions)
        , \game -> a [ href ("/game/play/" ++ String.fromInt game.id) ] [ text "Play" ]
        ]


viewJoinableGamesList : Model -> Html Msg
viewJoinableGamesList model =
    viewHtmlTable model.joinableGames
        [ "Game Id", "Game type", "White Player", "Black Player", "Join?" ]
        [ \game -> text (String.fromInt game.id)
        , \game -> text game.gameType
        , \game -> text game.whitePlayer
        , \game -> text game.blackPlayer
        , \game -> viewPrimaryButton ( "Join", JoinGame game.id )
        ]
