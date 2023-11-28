module ServerAdmin exposing (..)

import Browser
import Fuzz exposing (result)
import Html exposing (Html, button, div, h3, input, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (placeholder, style, type_)
import Html.Events exposing (onClick, onInput)
import Http
import Iso8601
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


serverURL : String
serverURL =
    "http://localhost:8080"


listUsersURL : String
listUsersURL =
    serverURL ++ "/users"


registerUserURL : String
registerUserURL =
    serverURL ++ "/register"


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
        (Decode.field "creation_time" (Decode.string |> Decode.andThen parseTime))
        (Decode.field "tokens" (Decode.list Decode.string))


parseTime : String -> Decode.Decoder (Maybe Time.Posix)
parseTime timeString =
    case Iso8601.toTime timeString of
        Ok posixTime ->
            Decode.succeed (Just posixTime)

        Err _ ->
            Decode.succeed Nothing


type alias Model =
    { username : String
    , password : String
    , token : String
    , error : String
    , users : List User
    , zone : Maybe Zone
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { username = ""
      , password = ""
      , token = ""
      , error = ""
      , users = []
      , zone = Nothing
      }
    , Cmd.batch [ loadUsersRequest, Task.perform ReceiveTimeZone Time.here ]
    )


type Msg
    = UpdateUsername String
    | UpdatePassword String
    | Register
    | RegisterResult (Result Http.Error String)
    | LoadUsers
    | LoadUsersResult (Result Http.Error (List User))
    | ReceiveTimeZone Zone


tokenDecoder : Decode.Decoder String
tokenDecoder =
    Decode.field "token" Decode.string


registerRequest : Model -> Cmd Msg
registerRequest model =
    let
        body =
            Encode.object
                [ ( "username", Encode.string model.username )
                , ( "password", Encode.string model.password )
                ]
    in
    Http.post
        { url = registerUserURL
        , body = Http.jsonBody body
        , expect = Http.expectJson RegisterResult tokenDecoder
        }


loadUsersRequest : Cmd Msg
loadUsersRequest =
    Http.get
        { url = listUsersURL
        , expect = Http.expectJson LoadUsersResult (Decode.list userDecoder)
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateUsername username ->
            ( { model | username = username }, Cmd.none )

        UpdatePassword password ->
            ( { model | password = password }, Cmd.none )

        Register ->
            -- Here you would send a request to the server to register the user
            ( { model | token = "", error = "" }, registerRequest model )

        RegisterResult result ->
            case result of
                Ok token ->
                    -- Registration was successful, update the token
                    ( { model | token = token }, loadUsersRequest )

                Err _ ->
                    -- An error occurred, update the error message
                    ( { model | error = "Registration failed" }, Cmd.none )

        LoadUsers ->
            ( model, loadUsersRequest )

        LoadUsersResult result ->
            case result of
                Ok users ->
                    -- Load users was successful
                    ( { model | users = users }, Cmd.none )

                Err _ ->
                    -- An error occurred
                    ( { model | error = "Failed to load users" }, Cmd.none )

        ReceiveTimeZone zone ->
            ( { model | zone = Just zone }, Cmd.none )



-- VIEW


viewRegisterUser : Model -> Html Msg
viewRegisterUser model =
    let
        tokenDiv =
            if model.token /= "" then
                [ div [] [ text ("Token: " ++ model.token) ] ]

            else
                []

        errorDiv =
            if model.error /= "" then
                [ div [] [ text ("Error: " ++ model.error) ] ]

            else
                []
    in
    div []
        ([ h3 [] [ text "Register a new user" ]
         , input [ placeholder "Username", type_ "text", onInput UpdateUsername ] []
         , input [ placeholder "Password", type_ "password", onInput UpdatePassword ] []
         , button [ onClick Register ] [ text "Register" ]
         ]
            ++ tokenDiv
            ++ errorDiv
        )


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
        , h3 [] [ text "List all users" ]
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
