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


type alias Model =
    { board : GipfBoard.Model
    , gameId : Int
    , token : String
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
            , token = token
            , error = Nothing
            }
    in
    ( model
    , joinGame model
    )


init : () -> ( Model, Cmd Msg )
init _ =
    initWithGameIdToken 1 "faa71988e5c4b226cd7607a7746e644a"


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
            , token = model.token
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
                , ( "signature", Encode.string (messageSignature a model.token) )
                ]

        message =
            { gameId = model.gameId
            , token = model.token
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
                        "Join" ->
                            let
                                ( newGipfBoard, gipfBoardCmd ) =
                                    GipfBoard.initFromString webSocketMessage.message
                            in
                            ( { model | board = newGipfBoard }, Cmd.map GipfBoardMsg gipfBoardCmd )

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
        , Html.map GipfBoardMsg (GipfBoard.view model.board)
        ]


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> messageReceiver WebSocketMessageReceived
        }
