module ServerAdmin exposing (..)

import Browser
import Debug
import Fuzz exposing (result)
import Html exposing (Html, button, div, h3, input, label, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (placeholder, style, type_)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Task
import Time exposing (Month(..), Zone)


{-|

    This server provides UX to assist with server management.

-}
main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }


type alias Model =
    { username : String
    , password : String
    , newPassword : String
    , userResponse : String
    , users : List User
    , games : List Game
    , zone : Maybe Zone
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { username = ""
      , password = ""
      , newPassword = ""
      , userResponse = ""
      , users = []
      , games = []
      , zone = Nothing
      }
    , Cmd.batch
        [ loadUsersRequest
        , loadGamesRequest
        , Task.perform
            ReceiveTimeZone
            Time.here
        ]
    )


serverURL : String
serverURL =
    "http://localhost:8080"


tokenDecoder : Decode.Decoder String
tokenDecoder =
    Decode.field "token" Decode.string


userRequest : Model -> String -> Cmd Msg
userRequest model myUrl =
    Http.post
        { url = myUrl
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "username", Encode.string model.username )
                    , ( "password", Encode.string model.password )
                    , ( "new_password", Encode.string model.newPassword )
                    ]
                )
        , expect = Http.expectJson UserResult tokenDecoder
        }


listUsersURL : String
listUsersURL =
    serverURL ++ "/users"


type alias User =
    { id : Int
    , username : String
    , creation_time : Maybe Time.Posix
    , tokens : List String
    }


userDecoder : Decode.Decoder User
userDecoder =
    Decode.map4 User
        (Decode.field "id" Decode.int)
        (Decode.field "username" Decode.string)
        (Decode.field "creation_time" (Decode.int |> Decode.map (Just << Time.millisToPosix)))
        (Decode.field "tokens" (Decode.list Decode.string))


loadUsersRequest : Cmd Msg
loadUsersRequest =
    Http.get
        { url = serverURL ++ "/users"
        , expect = Http.expectJson LoadUsersResult (Decode.list userDecoder)
        }


type alias Game =
    { id : Int
    , type_ : String
    , whiteUser : String
    , blackUser : String
    , whiteToken : String
    , blackToken : String
    , viewerToken : String
    , gameOver : Bool
    , gameResult : String
    , creationTime : Maybe Time.Posix
    }



-- gameDecoder : Decode.Decoder Game
-- gameDecoder =
--     Decode.map4
--         (\field1 field2 field3 field4 ->
--             Decode.map4
--                 (\field5 field6 field7 field8 ->
--                     Decode.map2
--                         (\field9 field10 ->
--                             { field1 = field1
--                             , field2 = field2
--                             , field3 = field3
--                             , field4 = field4
--                             , field5 = field5
--                             , field6 = field6
--                             , field7 = field7
--                             , field8 = field8
--                             , field9 = field9
--                             , field10 = field10
--                             }
--                         )
--                         (Decode.field "game_result" Decode.string)
--                         (Decode.field "creation_time" (Decode.nullable (Decode.int |> Decode.map (Just << Time.millisToPosix))))
--                 )
--                 (Decode.field "white_token" Decode.string)
--                 (Decode.field "black_token" Decode.string)
--                 (Decode.field "viewer_token" Decode.string)
--                 (Decode.field "game_over" Decode.bool)
--         )
--         (Decode.field "white_token" Decode.int)
--         (Decode.field "type" Decode.string)
--         (Decode.field "white_user" Decode.string)
--         (Decode.field "black_user" Decode.string)


gameDecoder : Decode.Decoder Game
gameDecoder =
    Decode.succeed Game
        |> required "id" Decode.int
        |> required "type" Decode.string
        |> required "white_user" Decode.string
        |> required "black_user" Decode.string
        |> required "white_token" Decode.string
        |> required "black_token" Decode.string
        |> required "viewer_token" Decode.string
        |> required "game_over" Decode.bool
        |> required "game_result" Decode.string
        |> required "creation_time" (Decode.int |> Decode.map (Just << Time.millisToPosix))


required : String -> Decode.Decoder a -> Decode.Decoder (a -> b) -> Decode.Decoder b
required field decoder =
    Decode.andThen (\next -> Decode.field field decoder |> Decode.map next)



-- optional : String -> Decode.Decoder a -> a -> Decode.Decoder (a -> b) -> Decode.Decoder b
-- optional field decoder default =
--     Decode.andThen (\next -> Decode.oneOf [ Decode.field field decoder, Decode.succeed default ] |> Decode.map next)


loadGamesRequest : Cmd Msg
loadGamesRequest =
    Http.get
        { url = serverURL ++ "/games"
        , expect = Http.expectJson LoadGamesResult (Decode.list gameDecoder)
        }


type Msg
    = UpdateUsername String
    | UpdatePassword String
    | UpdateNewPassword String
    | Register
    | Authenticate
    | ChangePassword
    | UserResult (Result Http.Error String)
    | LoadUsersResult (Result Http.Error (List User))
    | LoadGamesResult (Result Http.Error (List Game))
    | ReceiveTimeZone Zone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateUsername username ->
            ( { model | username = username }, Cmd.none )

        UpdatePassword password ->
            ( { model | password = password }, Cmd.none )

        UpdateNewPassword newPassword ->
            ( { model | newPassword = newPassword }, Cmd.none )

        Register ->
            -- Here you would send a request to the server to register the user
            ( { model | userResponse = "" }, userRequest model (serverURL ++ "/register") )

        Authenticate ->
            -- Here you would send a request to the server to authorize the user
            ( { model | userResponse = "" }, userRequest model (serverURL ++ "/authenticate") )

        ChangePassword ->
            -- Here you would send a request to the server to change the user's password
            ( { model | userResponse = "" }, userRequest model (serverURL ++ "/changepassword") )

        UserResult result ->
            case result of
                Ok token ->
                    -- Registration was successful, update the token
                    ( { model | userResponse = "token: " ++ token }, loadUsersRequest )

                Err error ->
                    -- An error occurred, update the error message
                    ( { model | userResponse = "error: " ++ Debug.toString error }, Cmd.none )

        LoadUsersResult result ->
            case result of
                Ok users ->
                    -- Load users was successful
                    ( { model | users = users }, Cmd.none )

                Err error ->
                    -- An error occurred
                    ( { model | userResponse = "Failed to load users: " ++ Debug.toString error }, Cmd.none )

        LoadGamesResult result ->
            case result of
                Ok games ->
                    -- Load games was successful
                    ( { model | games = games }, Cmd.none )

                Err error ->
                    -- An error occurred
                    ( { model | userResponse = "Failed to load games: " ++ Debug.toString error }, Cmd.none )

        ReceiveTimeZone zone ->
            ( { model | zone = Just zone }, Cmd.none )



-- VIEW


hBlock : Html msg
hBlock =
    div [ style "display" "inline-block", style "width" "10px" ] []


vBlock : Html msg
vBlock =
    div [ style "margin-top" "10px" ] []


viewInput : String -> (String -> Msg) -> Html Msg
viewInput lbl msg =
    div []
        [ label [] [ text lbl ]
        , hBlock
        , input [ placeholder lbl, type_ "text", onInput msg ] []
        ]


viewRegisterUser : Model -> Html Msg
viewRegisterUser model =
    div []
        [ h3 [] [ text "User operations" ]
        , viewInput "Username" UpdateUsername
        , vBlock
        , viewInput "Password" UpdatePassword
        , vBlock
        , viewInput "New Password" UpdateNewPassword
        , vBlock
        , button [ onClick Register ] [ text "Register" ]
        , hBlock
        , button [ onClick Authenticate ] [ text "Authenticate" ]
        , hBlock
        , button [ onClick ChangePassword ] [ text "Change" ]
        , vBlock
        , text model.userResponse
        ]


monthNumber : Time.Month -> Int
monthNumber month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


localTimeString : Maybe Zone -> Maybe Time.Posix -> String
localTimeString z time =
    case ( z, time ) of
        ( Just zone, Just t ) ->
            let
                hour =
                    Time.toHour zone t

                minute =
                    Time.toMinute zone t

                second =
                    Time.toSecond zone t

                day =
                    Time.toDay zone t

                month =
                    Time.toMonth zone t

                year =
                    Time.toYear zone t
            in
            String.fromInt hour
                ++ ":"
                ++ String.fromInt minute
                ++ ":"
                ++ String.fromInt second
                ++ " "
                ++ String.fromInt day
                ++ "/"
                ++ String.fromInt (monthNumber month)
                ++ "/"
                ++ String.fromInt year

        _ ->
            "Invalid time"


viewUser : Maybe Zone -> User -> Html Msg
viewUser z user =
    tr []
        [ td [] [ text (String.fromInt user.id) ]
        , td [] [ text user.username ]
        , td [] [ text (localTimeString z user.creation_time) ]
        , td [] [ text (String.join ", " user.tokens) ]
        ]


viewGame : Maybe Zone -> Game -> Html Msg
viewGame z game =
    tr []
        [ td [] [ text (String.fromInt game.id) ]
        , td [] [ text (localTimeString z game.creationTime) ]
        , td [] [ text game.whiteToken ]
        , td [] [ text game.blackToken ]
        , td [] [ text game.viewerToken ]
        ]


view : Model -> Html Msg
view model =
    div [ style "margin" "10px" ]
        [ viewRegisterUser model
        , h3 [] [ text "Users" ]
        , table []
            [ thead []
                [ tr []
                    [ th [] [ text "ID" ]
                    , th [] [ text "Username" ]
                    , th [] [ text "Creation Time" ]
                    , th [] [ text "Tokens" ]
                    ]
                ]
            , tbody [] (List.map (viewUser model.zone) model.users)
            ]
        , h3 [] [ text "Games" ]
        , table []
            [ thead []
                [ tr []
                    [ th [] [ text "ID" ]
                    , th [] [ text "Creation Time" ]
                    , th [] [ text "White token" ]
                    , th [] [ text "Black token" ]
                    , th [] [ text "Viewer token" ]
                    ]
                ]
            , tbody [] (List.map (viewGame model.zone) model.games)
            ]
        ]
