port module Server exposing
    ( Game
    , ServerResponse(..)
    , User
    , WebSocketMessage
    , authenticate
    , isAuthenticated
    , messageReceiver
    , sendWebSocketMessage
    )

{-|

    This module is used to abstract away the communication with the server.

    It defines two ports, sendMessage and messageReceiver, which are used to send an encoded message
    to the server and receive messages from the server, respectively.

    Received message needs to be further decoded, depending on the messageType field.
    It can contain one of the following messages:
    - a full list of signed actions up until this point (responding to message type "SendFullGame")
    - a single action, broadcased to all players when a new action is made (responding to message type "Move")
    - a notification that the game has been terminated (as a response to incorrect signature)
    - some other generic error message

-}

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import MD5


{-|

        Raw ports and server message decoding

-}
type alias Token =
    String


type alias WebSocketMessage =
    { gameId : Int
    , token : Token
    , messageType : String
    , message : String
    }


port sendMessage : Encode.Value -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg


webSocketMessageDecoder : Decode.Decoder WebSocketMessage
webSocketMessageDecoder =
    Decode.map4 WebSocketMessage
        (Decode.field "game_id" Decode.int)
        (Decode.field "token" Decode.string)
        (Decode.field "message_type" Decode.string)
        (Decode.field "message" Decode.string)


webSocketMessageEncoder : WebSocketMessage -> Encode.Value
webSocketMessageEncoder message =
    Encode.object
        [ ( "game_id", Encode.int message.gameId )
        , ( "token", Encode.string message.token )
        , ( "message_type", Encode.string message.messageType )
        , ( "message", Encode.string message.message )
        ]


sendWebSocketMessage : WebSocketMessage -> Cmd msg
sendWebSocketMessage message =
    let
        encodedMessage =
            webSocketMessageEncoder message
    in
    sendMessage encodedMessage


{-|

    Interpreting a message

-}
type ServerResponse
    = Action SignedAction
    | FullGame (List SignedAction)
    | GameTerminated
    | Error String


type alias SignedAction =
    { action : Action
    , action_num : Int
    , signature : String
    }


messageSignature : Action -> String -> String
messageSignature action token =
    MD5.hex (String.fromInt action.num ++ action.action ++ token)


signAction : Action -> String -> Action
signAction action token =
    { action | signature = messageSignature action token }


checkSignature : Action -> String -> Bool
checkSignature action token =
    action.signature == messageSignature action token


{-|

    User

-}
type alias User =
    { username : String
    , password : String
    , loginToken : String
    , error : Maybe Http.Error
    }


isAuthenticated : User -> Bool
isAuthenticated user =
    user.loginToken /= "" && user.error == Nothing


authenticate : User -> (Result Http.Error String -> msg) -> Cmd msg
authenticate user msg =
    if isAuthenticated user then
        Cmd.none

    else
        Http.post
            { url = "http://localhost:8080/authenticate"
            , body =
                Http.jsonBody
                    (Encode.object
                        [ ( "username", Encode.string user.username )
                        , ( "password", Encode.string user.password )
                        ]
                    )
            , expect = Http.expectJson msg (Decode.field "token" Decode.string)
            }


{-|

    Game

-}
type alias Action =
    { num : Int
    , action : String
    , signature : String
    }


type alias Game =
    { id : Int
    , token : String
    , actions : List Action
    , error : Maybe Http.Error
    }


getGame : Int -> token -> (Result Http.Error Game -> msg) -> Cmd msg
getGame id token msg =
    Cmd.none
