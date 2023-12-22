module Game exposing
    ( Model
    , Msg(..)
    , init
    , update
    , updateModelWithUser
    , viewCreateNewGame
    , viewUserGameList
    )

import Auth
import Html exposing (Html, div, h2, input, label, option, select, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (checked, class, name, type_, value)
import Html.Events exposing (onCheck, onInput)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import ServerUtils exposing (HttpResult, parseResult, responseDecoder)
import Time exposing (Month(..))
import Ui exposing (viewErrorMessage, viewPrimaryButton)


type alias Model =
    { screenName : String
    , token : String
    , gameType : String
    , color : String
    , error : Maybe String
    , games : List Game
    }


init : Maybe Auth.User -> ( Model, Cmd Msg )
init maybeUser =
    updateModelWithUser maybeUser
        { screenName = ""
        , token = ""
        , gameType = "Basic GIPF"
        , color = "white"
        , error = Nothing
        , games = []
        }


updateModelWithUser : Maybe Auth.User -> Model -> ( Model, Cmd Msg )
updateModelWithUser maybeUser model =
    case maybeUser of
        Just user ->
            let
                newModel =
                    { model | screenName = user.screenName, token = user.token }
            in
            ( newModel, listGames newModel )

        Nothing ->
            ( { model | screenName = "", token = "", games = [] }, Cmd.none )


type Msg
    = SelectGameType String
    | SelectColor String
    | CreateGame
    | CreateGameReceived (HttpResult Game)
    | GameListReceived (HttpResult (List Game))
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


listGames : Model -> Cmd Msg
listGames model =
    if model.token == "" then
        Cmd.none

    else
        Http.post
            { url = "/game/list"
            , body =
                Http.jsonBody <|
                    Encode.object
                        [ ( "token", Encode.string model.token ) ]
            , expect = Http.expectJson GameListReceived (responseDecoder gameListDecoder)
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
                    ( { model | error = Nothing }, listGames model )

                Err error ->
                    ( { model | error = Just error }, Cmd.none )

        GameListReceived result ->
            case parseResult result of
                Ok games ->
                    ( { model | games = games }, Cmd.none )

                Err error ->
                    ( { model | error = Just error }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


viewCreateNewGame : Model -> Html Msg
viewCreateNewGame model =
    if model.screenName == "" then
        div [] []

    else
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
                            , checked (model.color == "white")
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
                            , checked (model.color == "black")
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
            , viewErrorMessage model.error
            ]


viewUserGameList : Model -> Html Msg
viewUserGameList model =
    -- Create a table of games, with the following fields:
    -- - Game type, with a link to the game
    -- - Opponent name
    -- - Game status
    -- - Game result
    div [ class "p-4" ]
        [ h2 [ class "text-lg font-bold mb-4" ] [ text "Your games" ]
        , div [ class "flex flex-col" ]
            [ div [ class "overflow-x-auto" ]
                [ table [ class "table-auto" ]
                    [ thead []
                        [ tr []
                            [ th [ class "px-4 py-2" ] [ text "Game Id" ]
                            , th [ class "px-4 py-2" ] [ text "Game type" ]
                            , th [ class "px-4 py-2" ] [ text "White Player" ]
                            , th [ class "px-4 py-2" ] [ text "Black Player" ]
                            , th [ class "px-4 py-2" ] [ text "Num Actions" ]
                            ]
                        ]
                    , tbody []
                        (List.map
                            (\game ->
                                tr []
                                    [ td [ class "border px-4 py-2" ]
                                        [ text (String.fromInt game.id) ]
                                    , td [ class "border px-4 py-2" ]
                                        [ text game.gameType ]
                                    , td [ class "border px-4 py-2" ]
                                        [ text game.whitePlayer ]
                                    , td [ class "border px-4 py-2" ]
                                        [ text game.blackPlayer ]
                                    , td [ class "border px-4 py-2" ]
                                        [ text (String.fromInt game.numActions) ]
                                    ]
                            )
                            model.games
                        )
                    ]
                ]
            ]
        ]
