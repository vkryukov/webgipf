port module PlayGame exposing (..)

import GipfBoard
import Html exposing (Html, div, em, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import MD5
import Platform.Cmd as Cmd
import Ui exposing (viewBoldText)


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
    , gameType : String
    , whitePlayer : String
    , blackPlayer : String
    , thisPlayer : String
    , state : GameState
    , error : Maybe String
    }


initWithGameIdToken : Int -> String -> ( Model, Cmd Msg )
initWithGameIdToken gameId token =
    let
        ( model, _ ) =
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
            , gameType = ""
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
                                        , gameType = gameResponse.gameType
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

                gameOver =
                    GipfBoard.gameIsOver newBoard

                gameOverCmd =
                    if gameOver /= "" then
                        sendGameIsOver model gameOver

                    else
                        Cmd.none
            in
            ( { model | board = GipfBoard newBoard }, Cmd.batch [ Cmd.map GipfBoardMsg cmd, gameOverCmd ] )

        _ ->
            ( model, Cmd.none )


sendGameIsOver : Model -> String -> Cmd Msg
sendGameIsOver model status =
    let
        message =
            { gameId = model.gameId
            , token = model.playerToken
            , messageType = "GameOver"
            , message = status
            }
    in
    sendMessage (webSocketMessageEncoder message)


viewError : Model -> Html Msg
viewError model =
    case model.error of
        Just error ->
            div [] [ text error ]

        Nothing ->
            div [] []


viewGameInfo : Model -> Html Msg
viewGameInfo model =
    let
        whitePlayer =
            if model.thisPlayer == "white" then
                "You"

            else
                model.whitePlayer

        blackPlayer =
            if model.thisPlayer == "black" then
                "You"

            else
                model.blackPlayer
    in
    div [ class "flex mt-2 ml-4" ]
        [ div [ class "text-gray-500" ] [ viewBoldText model.gameType ]
        , div [ class "w-8" ] []
        , div [ class "flex" ]
            [ viewBoldText whitePlayer, div [ class "w-1" ] [], text "(white)" ]
        , div [ class "flex mx-4" ] [ em [] [ text " vs. " ] ]
        , div
            [ class "flex" ]
            [ viewBoldText blackPlayer, div [ class "w-1" ] [], text "(black)" ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ viewError model
        , case model.state of
            JoinRequestSent ->
                div [] [ text "Joining..." ]

            Joined ->
                div [ class "w-full justify-center items-center" ]
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
