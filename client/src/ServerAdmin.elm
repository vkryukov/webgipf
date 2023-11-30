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
    , zone : Maybe Zone
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { username = ""
      , password = ""
      , newPassword = ""
      , userResponse = ""
      , users = []
      , zone = Nothing
      }
    , Cmd.batch [ loadUsersRequest, Task.perform ReceiveTimeZone Time.here ]
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


type Msg
    = UpdateUsername String
    | UpdatePassword String
    | UpdateNewPassword String
    | Register
    | Authenticate
    | ChangePassword
    | UserResult (Result Http.Error String)
    | LoadUsers
    | LoadUsersResult (Result Http.Error (List User))
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

        LoadUsers ->
            ( model, loadUsersRequest )

        LoadUsersResult result ->
            case result of
                Ok users ->
                    -- Load users was successful
                    ( { model | users = users }, Cmd.none )

                Err error ->
                    -- An error occurred
                    ( { model | userResponse = "Failed to load users: " ++ Debug.toString error }, Cmd.none )

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


viewUser : Maybe Zone -> User -> Html Msg
viewUser z user =
    let
        localTimeString =
            case ( z, user.creation_time ) of
                ( Just zone, Just time ) ->
                    let
                        hour =
                            Time.toHour zone time

                        minute =
                            Time.toMinute zone time

                        second =
                            Time.toSecond zone time

                        day =
                            Time.toDay zone time

                        month =
                            Time.toMonth zone time

                        year =
                            Time.toYear zone time
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
    in
    tr []
        [ td [] [ text (String.fromInt user.id) ]
        , td [] [ text user.username ]
        , td [] [ text localTimeString ]
        , td [] [ text (String.join ", " user.tokens) ]
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
        ]
