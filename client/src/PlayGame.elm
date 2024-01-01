port module PlayGame exposing (..)

import Browser
import GipfBoard
import Html exposing (Html, div, text)
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import MD5
import Platform.Cmd as Cmd


port sendMessage : Encode.Value -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg


type GameState
    = JoinRequestSent
    | Joined



-- Generic interface to various game boards


{-|

    GameInit takes game type and actions as strings,
    and returns a game board.

-}
initGame : String -> String -> String -> GameBoard
initGame gameType actions player =
    if String.startsWith "GIPF" gameType then
        GipfBoard (GipfBoard.initWithPlayer gameType actions player)

    else
        UnimplementedBoard


type GameBoard
    = GipfBoard GipfBoard.Model
    | UnimplementedBoard



-- Main model


type alias Model =
    { board : GameBoard
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
        model =
            { board = UnimplementedBoard
            , gameId = 0
            , playerToken = ""
            , gameToken = ""
            , whitePlayer = ""
            , blackPlayer = ""
            , thisPlayer = ""
            , state = JoinRequestSent
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


webSocketMessageDecoder : Decode.Decoder WebSocketMessage
webSocketMessageDecoder =
    Decode.succeed WebSocketMessage
        |> Pipeline.required "game_id" Decode.int
        |> Pipeline.required "token" Decode.string
        |> Pipeline.required "message_type" Decode.string
        |> Pipeline.required "message" Decode.string


{-| Processing actions
-}
type alias Action =
    { actionNum : Int
    , action : String
    , signature : String
    }


actionDecoder : Decode.Decoder Action
actionDecoder =
    Decode.succeed Action
        |> Pipeline.required "action_num" Decode.int
        |> Pipeline.required "action" Decode.string
        |> Pipeline.required "signature" Decode.string


type alias JoinGameResponse =
    { player : String
    , gameToken : String
    , whitePlayer : String
    , blackPlayer : String
    , gameType : String
    , actions : List Action
    }


gameResponseDecoder : Decode.Decoder JoinGameResponse
gameResponseDecoder =
    Decode.succeed JoinGameResponse
        |> Pipeline.required "player" Decode.string
        |> Pipeline.required "game_token" Decode.string
        |> Pipeline.required "white_player" Decode.string
        |> Pipeline.required "black_player" Decode.string
        |> Pipeline.required "game_type" Decode.string
        |> Pipeline.required "actions" (Decode.oneOf [ Decode.list actionDecoder, Decode.succeed [] ])


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WebSocketMessageReceived message ->
            case Decode.decodeString webSocketMessageDecoder message of
                Ok webSocketMessage ->
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
                                            -- TODO: we don't need to splt the actions to a list on the server side
                                            String.join " " (List.map (\a -> a.action) gameResponse.actions)

                                        board =
                                            initGame gameResponse.gameType actions gameResponse.player
                                    in
                                    ( { model
                                        | gameToken = gameResponse.gameToken
                                        , whitePlayer = gameResponse.whitePlayer
                                        , blackPlayer = gameResponse.blackPlayer
                                        , thisPlayer = gameResponse.player
                                        , state = Joined
                                        , board = board
                                      }
                                    , Cmd.none
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
                                    sendActionToBoard model action

                                Err err ->
                                    ( { model | error = Just (Decode.errorToString err) }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Err err ->
                    ( { model | error = Just (Decode.errorToString err) }, Cmd.none )

        message ->
            updateBoard message model


updateBoard : Msg -> Model -> ( Model, Cmd Msg )
updateBoard someMessage model =
    case ( someMessage, model.board ) of
        ( GipfBoardMsg msg, GipfBoard board ) ->
            let
                ( newBoard, cmd ) =
                    GipfBoard.update msg board

                maybeLastAction =
                    GipfBoard.getActionToSend msg newBoard

                cmds =
                    case maybeLastAction of
                        Just lastAction ->
                            [ sendAction model lastAction, Cmd.map GipfBoardMsg cmd ]

                        Nothing ->
                            [ Cmd.map GipfBoardMsg cmd ]
            in
            ( { model | board = GipfBoard newBoard }, Cmd.batch cmds )

        _ ->
            ( model, Cmd.none )


sendActionToBoard : Model -> Action -> ( Model, Cmd Msg )
sendActionToBoard model action =
    case model.board of
        GipfBoard board ->
            let
                ( newBoard, cmd ) =
                    GipfBoard.receiveAction board action.action
            in
            ( { model | board = GipfBoard newBoard }, Cmd.map GipfBoardMsg cmd )

        _ ->
            ( model, Cmd.none )


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
        [ div [] [ text ("White player: " ++ model.whitePlayer) ]
        , div [] [ text ("Black player: " ++ model.blackPlayer) ]
        , div [] [ text ("This player: " ++ model.thisPlayer) ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ viewError model
        , case model.state of
            JoinRequestSent ->
                div [] [ text "Joining..." ]

            Joined ->
                div []
                    [ viewGameInfo model
                    , viewBoard model.board
                    ]
        ]


viewBoard : GameBoard -> Html Msg
viewBoard someBoard =
    case someBoard of
        GipfBoard board ->
            Html.map GipfBoardMsg (GipfBoard.view board)

        _ ->
            div [] [ text "Unimplemented board" ]


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
