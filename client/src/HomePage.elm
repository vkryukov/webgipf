module HomePage exposing
    ( Model
    , Msg(..)
    , cancelGame
    , init
    , update
    , updateModelWithUser
    , viewCreateNewGame
    , viewJoinableGamesList
    , viewOwnGamesList
    )

import Auth
import Html exposing (Html, a, div, label, option, p, select, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onInput)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Routes
import ServerUtils exposing (HttpResult, parseResult, responseDecoder)
import Time exposing (Month(..))
import Ui exposing (viewErrorMessage, viewHtmlTable, viewRadio, viewSecondaryButton, viewSmallPrimaryButton)


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
        , gameType = "GIPF basic"
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
    | CancelGame Int
    | GameCancelled (Result Http.Error ())
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
    Decode.succeed Game
        |> Pipeline.required "id" Decode.int
        |> Pipeline.required "type" Decode.string
        |> Pipeline.required "white_player" Decode.string
        |> Pipeline.required "black_player" Decode.string
        |> Pipeline.required "white_token" Decode.string
        |> Pipeline.required "black_token" Decode.string
        |> Pipeline.required "num_actions" Decode.int


gameListDecoder : Decode.Decoder (List Game)
gameListDecoder =
    Decode.oneOf
        [ Decode.null []
        , Decode.list gameDecoder
        ]


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
                    , ( "public", Encode.bool True )
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


cancelGame : Int -> Model -> Cmd Msg
cancelGame gameId model =
    Http.post
        { url = "/game/cancel"
        , body =
            Http.jsonBody <|
                Encode.object
                    [ ( "token", Encode.string model.token )
                    , ( "id", Encode.int gameId )
                    ]
        , expect = Http.expectWhatever GameCancelled
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

        CancelGame gameId ->
            ( model, cancelGame gameId model )

        GameCancelled _ ->
            ( model, ownGames model )

        NoOp ->
            ( model, Cmd.none )


viewCreateNewGame : Model -> Html Msg
viewCreateNewGame model =
    if model.screenName == "" then
        div [] []

    else
        div []
            [ p [ class "mb-2" ]
                [ text """You can create a new game here. Currently supported: 
                1) GIPF basic (no GIPF pieces), 
                2) GIPF standard (GIPF pieces in a standard starting position), and 
                3) GIPF tournament (decide where to place GIPF pieces initially and how many).""" ]
            , div
                [ class "flex items-center space-x-4 bg-cyan-100 rounded p-3" ]
                [ label [ class "mr-2" ] [ text "Game type:" ]
                , select [ class "form-select", onInput SelectGameType ]
                    [ option [] [ text "GIPF basic" ]
                    , option [] [ text "GIPF standard" ]
                    , option [] [ text "GIPF tournament" ]
                    ]
                , label [ class "ml-4 mr-2" ] [ text "Play as:" ]
                , div [ class "flex items-center space-x-4" ]
                    [ viewRadio (model.color == "white") "White" (SelectColor "white") NoOp
                    , viewRadio (model.color == "black") "Black" (SelectColor "black") NoOp
                    ]
                , viewSmallPrimaryButton ( "Create game", CreateGame )
                ]
            , viewErrorMessage model.error
            ]


viewOwnGamesList : Model -> Html Msg
viewOwnGamesList model =
    viewHtmlTable model.ownGames
        [ "Game Id", "Game type", "White Player", "Black Player", "Num Actions", "" ]
        [ \game -> text (String.fromInt game.id)
        , \game -> text game.gameType
        , \game -> text game.whitePlayer
        , \game -> text game.blackPlayer
        , \game -> text (String.fromInt game.numActions)
        , \game ->
            if (game.whitePlayer /= "") && (game.blackPlayer /= "") then
                a
                    [ Routes.href (Routes.PlayGame game.id)
                    , class "text-blue-500 hover:text-blue-700 underline"
                    ]
                    [ text "Play" ]

            else
                viewSecondaryButton ( "Cancel", CancelGame game.id )
        ]


viewJoinableGamesList : Model -> Html Msg
viewJoinableGamesList model =
    if model.joinableGames == [] then
        div [] [ text "No games to join! Start a new game or wait for someone else to start one." ]

    else
        viewHtmlTable model.joinableGames
            [ "Game Id", "Game type", "White Player", "Black Player", "" ]
            [ \game -> text (String.fromInt game.id)
            , \game -> text game.gameType
            , \game -> text game.whitePlayer
            , \game -> text game.blackPlayer
            , \game -> viewSecondaryButton ( "Join", JoinGame game.id )
            ]
