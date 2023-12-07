port module Main exposing (..)

import Browser
import Gipf
import GipfBoard
import Html exposing (Html, div, text)
import Json.Decode as Decode
import Json.Encode as Encode
import MD5


port sendMessage : Encode.Value -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg


type GameState
    = WaitingToJoin
    | Joined


type alias Model =
    { board : GipfBoard.Model
    , gameId : Int
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
        ( board, _ ) =
            GipfBoard.initFromString ""

        model =
            { board = board
            , gameId = gameId
            , playerToken = token
            , gameToken = ""
            , whitePlayer = ""
            , blackPlayer = ""
            , thisPlayer = ""
            , state = WaitingToJoin
            , error = Nothing
            }
    in
    ( model
    , joinGame model
    )


init : () -> ( Model, Cmd Msg )
init _ =
    initWithGameIdToken 1 "f0077ae4f67ecb3aeb6e477865e71ea8"


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
        (Decode.field "actions" (Decode.list actionDecoder))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GipfBoardMsg gipfBoardMsg ->
            let
                ( newGipfBoard, gipfBoardCmd ) =
                    GipfBoard.update gipfBoardMsg model.board

                lastAction =
                    Gipf.lastAction newGipfBoard.game
            in
            case gipfBoardMsg of
                GipfBoard.MoveMade _ ->
                    ( { model | board = newGipfBoard }, sendAction model lastAction )

                GipfBoard.RemovePieces ->
                    ( { model | board = newGipfBoard }, sendAction model lastAction )

                _ ->
                    ( { model | board = newGipfBoard }, Cmd.map GipfBoardMsg gipfBoardCmd )

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
                                            GipfBoard.initFromString actions
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


view : Model -> Html Msg
view model =
    div []
        [ viewError model
        , if model.state == WaitingToJoin then
            div [] [ text "Waiting to join" ]

          else
            Html.map GipfBoardMsg (GipfBoard.view model.board)
        ]


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> messageReceiver WebSocketMessageReceived
        }
