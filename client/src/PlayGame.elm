port module PlayGame exposing (..)

import Browser
import Gipf
import GipfBoard
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode
import MD5


port sendMessage : Encode.Value -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg


type GameState
    = EnterGameIdAndToken
    | JoinRequestSent
    | Joined


type alias Model =
    { board : GipfBoard.Model
    , gameId : Int
    , gameIdInput : String
    , playerToken : String
    , gameToken : String
    , whitePlayer : String
    , blackPlayer : String
    , thisPlayer : String
    , state : GameState
    , error : Maybe String
    }


initWithGameIdToken : Int -> String -> ( Model, Cmd Msg )
initWithGameIdToken gameId token =
    let
        ( model, cmd ) =
            init ()

        newModel =
            { model | gameId = gameId, playerToken = token, state = JoinRequestSent }
    in
    ( newModel
    , joinGame newModel
    )


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( board, _ ) =
            GipfBoard.initEmpty

        model =
            { board = board
            , gameId = 0
            , gameIdInput = ""
            , playerToken = ""
            , gameToken = ""
            , whitePlayer = ""
            , blackPlayer = ""
            , thisPlayer = ""
            , state = EnterGameIdAndToken
            , error = Nothing
            }
    in
    ( model
    , Cmd.none
    )


type alias WebSocketMessage =
    { gameId : Int
    , token : String
    , messageType : String
    , message : String
    }


webSocketMessageEncoder : WebSocketMessage -> Encode.Value
webSocketMessageEncoder message =
    Encode.object
        [ ( "game_id", Encode.int message.gameId )
        , ( "token", Encode.string message.token )
        , ( "message_type", Encode.string message.messageType )
        , ( "message", Encode.string message.message )
        ]


joinGame : Model -> Cmd Msg
joinGame model =
    let
        message =
            { gameId = model.gameId
            , token = model.playerToken
            , messageType = "Join"
            , message = ""
            }
    in
    sendMessage (webSocketMessageEncoder message)


messageSignature : ( Int, String ) -> String -> String
messageSignature a token =
    let
        ( actionNum, action ) =
            a
    in
    MD5.hex (String.fromInt actionNum ++ action ++ token)


sendAction : Model -> ( Int, String ) -> Cmd Msg
sendAction model a =
    let
        ( actionNum, action ) =
            a

        signedAction =
            Encode.object
                [ ( "action_num", Encode.int actionNum )
                , ( "action", Encode.string action )
                , ( "signature", Encode.string (messageSignature a model.playerToken) )
                ]

        message =
            { gameId = model.gameId
            , token = model.playerToken
            , messageType = "Action"
            , message = Encode.encode 0 signedAction
            }
    in
    sendMessage (webSocketMessageEncoder message)


type Msg
    = GipfBoardMsg GipfBoard.Msg
    | WebSocketMessageReceived String
    | UpdateGameId String
    | UpdatePlayerToken String
    | Submit


webSocketMessageDecoder : Decode.Decoder WebSocketMessage
webSocketMessageDecoder =
    Decode.map4 WebSocketMessage
        (Decode.field "game_id" Decode.int)
        (Decode.field "token" Decode.string)
        (Decode.field "message_type" Decode.string)
        (Decode.field "message" Decode.string)


{-| Processing actions
-}
type alias Action =
    { actionNum : Int
    , action : String
    , signature : String
    }


actionDecoder : Decode.Decoder Action
actionDecoder =
    Decode.map3 Action
        (Decode.field "action_num" Decode.int)
        (Decode.field "action" Decode.string)
        (Decode.field "signature" Decode.string)


type alias JoinGameResponse =
    { player : String
    , game_token : String
    , white_player : String
    , black_player : String
    , actions : List Action
    }


gameResponseDecoder : Decode.Decoder JoinGameResponse
gameResponseDecoder =
    Decode.map5 JoinGameResponse
        (Decode.field "player" Decode.string)
        (Decode.field "game_token" Decode.string)
        (Decode.field "white_player" Decode.string)
        (Decode.field "black_player" Decode.string)
        (Decode.field "actions" (Decode.oneOf [ Decode.list actionDecoder, Decode.succeed [] ]))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateGameId gameIdStr ->
            ( { model | gameIdInput = gameIdStr }, Cmd.none )

        UpdatePlayerToken token ->
            ( { model | playerToken = token }, Cmd.none )

        Submit ->
            let
                gameId =
                    String.toInt model.gameIdInput |> Maybe.withDefault 0

                _ =
                    Debug.log "gameIdInput" ( model.gameIdInput, gameId )
            in
            initWithGameIdToken gameId model.playerToken

        GipfBoardMsg gipfBoardMsg ->
            let
                ( newGipfBoard, gipfBoardCmd ) =
                    GipfBoard.update gipfBoardMsg model.board

                allowed =
                    GipfBoard.actionAllowed newGipfBoard model.thisPlayer

                _ =
                    Debug.log "allowed" ( newGipfBoard, model.thisPlayer, allowed )

                newGipfBoard1 =
                    { newGipfBoard | allowActions = allowed }

                lastAction =
                    Gipf.lastAction newGipfBoard1.game
            in
            case gipfBoardMsg of
                GipfBoard.MoveMade _ ->
                    ( { model | board = newGipfBoard1 }, sendAction model lastAction )

                GipfBoard.RemovePieces ->
                    ( { model | board = newGipfBoard1 }, sendAction model lastAction )

                _ ->
                    ( { model | board = newGipfBoard1 }, Cmd.map GipfBoardMsg gipfBoardCmd )

        WebSocketMessageReceived message ->
            case Decode.decodeString webSocketMessageDecoder message of
                Ok webSocketMessage ->
                    let
                        _ =
                            Debug.log "WebSocketMessageReceived" webSocketMessage
                    in
                    case webSocketMessage.messageType of
                        "GameJoined" ->
                            let
                                gameResult =
                                    Decode.decodeString gameResponseDecoder webSocketMessage.message
                            in
                            case gameResult of
                                Ok gameResponse ->
                                    let
                                        actions =
                                            String.join " " (List.map (\a -> a.action) gameResponse.actions)

                                        ( newGipfBoard, cmd ) =
                                            GipfBoard.initFromStringWithPlayer actions gameResponse.player
                                    in
                                    ( { model
                                        | gameToken = gameResponse.game_token
                                        , whitePlayer = gameResponse.white_player
                                        , blackPlayer = gameResponse.black_player
                                        , thisPlayer = gameResponse.player
                                        , state = Joined
                                        , board = newGipfBoard
                                      }
                                    , Cmd.map GipfBoardMsg cmd
                                    )

                                Err err ->
                                    ( { model | error = Just (Decode.errorToString err) }, Cmd.none )

                        "Action" ->
                            let
                                actionResult =
                                    Decode.decodeString actionDecoder webSocketMessage.message
                            in
                            case actionResult of
                                Ok action ->
                                    let
                                        ( newGipfBoard, cmd ) =
                                            GipfBoard.receiveAction model.board action.action

                                        allowed =
                                            GipfBoard.actionAllowed newGipfBoard model.thisPlayer

                                        newGipfBoard1 =
                                            { newGipfBoard | allowActions = allowed }
                                    in
                                    ( { model | board = newGipfBoard1 }, Cmd.map GipfBoardMsg cmd )

                                Err err ->
                                    ( { model | error = Just (Decode.errorToString err) }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Err err ->
                    ( { model | error = Just (Decode.errorToString err) }, Cmd.none )


viewError : Model -> Html Msg
viewError model =
    case model.error of
        Just error ->
            div [] [ text error ]

        Nothing ->
            div [] []


viewGameInfo : Model -> Html Msg
viewGameInfo model =
    div []
        [ div [] [ text ("Game ID: " ++ String.fromInt model.gameId) ]
        , div [] [ text ("Game token: " ++ model.gameToken) ]
        , div [] [ text ("White player: " ++ model.whitePlayer) ]
        , div [] [ text ("Black player: " ++ model.blackPlayer) ]
        , div [] [ text ("This player: " ++ model.thisPlayer) ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ viewError model
        , case model.state of
            EnterGameIdAndToken ->
                div []
                    [ input [ placeholder "Enter game ID", onInput UpdateGameId ] []
                    , input [ placeholder "Enter player token", onInput UpdatePlayerToken ] []
                    , button [ onClick Submit ] [ text "Submit" ]
                    ]

            JoinRequestSent ->
                div [] [ text "Joining..." ]

            Joined ->
                div []
                    [ viewGameInfo model
                    , Html.map GipfBoardMsg (GipfBoard.view model.board)
                    ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    messageReceiver WebSocketMessageReceived


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
