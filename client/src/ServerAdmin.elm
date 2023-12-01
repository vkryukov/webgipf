module ServerAdmin exposing (..)

import Browser
import Debug
import Fuzz exposing (result)
import Html exposing (Html, button, div, h3, input, label, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (placeholder, style, type_)
import Html.Events exposing (onCheck, onClick, onInput)
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
    , whiteUser : String
    , blackUser : String
    , isPublic : Bool
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
      , whiteUser = ""
      , blackUser = ""
      , isPublic = False
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
    | UpdateWhiteUser String
    | UpdateBlackUser String
    | LoadGamesResult (Result Http.Error (List Game))
    | ReceiveTimeZone Zone
    | CreateGame
    | UpdatePublicGame Bool
    | CreatedGame (Result Http.Error ())


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

        UpdateWhiteUser whiteUser ->
            ( { model | whiteUser = whiteUser }, Cmd.none )

        UpdateBlackUser blackUser ->
            ( { model | blackUser = blackUser }, Cmd.none )

        CreateGame ->
            let
                game =
                    { whiteUser = model.whiteUser
                    , blackUser = model.blackUser
                    , isPublic = model.isPublic
                    }
            in
            ( model
            , Http.post
                { url = serverURL ++ "/newgame"
                , body =
                    Http.jsonBody
                        (Encode.object
                            [ ( "type", Encode.string "standard" )
                            , ( "white_username", Encode.string game.whiteUser )
                            , ( "black_username", Encode.string game.blackUser )
                            , ( "public", Encode.bool game.isPublic )
                            ]
                        )
                , expect = Http.expectWhatever CreatedGame
                }
            )

        UpdatePublicGame isPublic ->
            ( { model | isPublic = isPublic }, Cmd.none )

        CreatedGame result ->
            case result of
                Ok _ ->
                    ( model, loadGamesRequest )

                Err error ->
                    ( { model | userResponse = "Failed to create game: " ++ Debug.toString error }, Cmd.none )



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


viewUserOperations : Model -> Html Msg
viewUserOperations model =
    div []
        [ viewInput "Username" UpdateUsername
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
        , td [] [ text (String.join ", " user.tokens) ]
        , td [] [ text (localTimeString z user.creation_time) ]
        ]


viewGame : Maybe Zone -> Game -> Html Msg
viewGame z game =
    tr []
        [ td [] [ text (String.fromInt game.id) ]
        , td [] [ text game.whiteUser ]
        , td [] [ text game.blackUser ]
        , td [] [ text game.whiteToken ]
        , td [] [ text game.blackToken ]
        , td [] [ text game.viewerToken ]
        , td [] [ text (localTimeString z game.creationTime) ]
        ]


viewGameOperations : Model -> Html Msg
viewGameOperations model =
    div []
        [ viewInput "White user" UpdateWhiteUser
        , vBlock
        , viewInput "Black user" UpdateBlackUser
        , vBlock
        , div []
            [ input [ type_ "checkbox", onCheck UpdatePublicGame ] []
            , text " Public Game"
            ]
        , vBlock
        , button [ onClick CreateGame ] [ text "Create" ]
        ]


view : Model -> Html Msg
view model =
    div [ style "margin" "10px" ]
        [ h3 [] [ text "Users" ]
        , viewUserOperations model
        , table []
            [ thead []
                [ tr []
                    [ th [] [ text "ID" ]
                    , th [] [ text "Username" ]
                    , th [] [ text "Tokens" ]
                    , th [] [ text "Creation Time" ]
                    ]
                ]
            , tbody [] (List.map (viewUser model.zone) model.users)
            ]
        , h3 [] [ text "Games" ]
        , viewGameOperations model
        , vBlock
        , table []
            [ thead []
                [ tr []
                    [ th [] [ text "ID" ]
                    , th [] [ text "White user" ]
                    , th [] [ text "Black user" ]
                    , th [] [ text "White token" ]
                    , th [] [ text "Black token" ]
                    , th [] [ text "Viewer token" ]
                    , th [] [ text "Creation Time" ]
                    ]
                ]
            , tbody [] (List.map (viewGame model.zone) model.games)
            ]
        ]
