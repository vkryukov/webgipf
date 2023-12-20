module Game exposing
    ( Model
    , Msg(..)
    , init
    , update
    , updateModelWithUser
    , viewCreateNewGame
    )

import Auth
import Html exposing (Html, div, h2, input, label, option, select, text)
import Html.Attributes exposing (checked, class, name, type_, value)
import Html.Events exposing (onCheck, onInput)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import ServerUtils exposing (HttpResult, parseResult, responseDecoder)
import Time exposing (Month(..))
import Ui exposing (viewErrorMessage, viewPrimaryButton)


type alias Model =
    { screenName : String
    , gameType : String
    , color : String
    , error : Maybe String
    }


init : Maybe Auth.User -> ( Model, Cmd Msg )
init maybeUser =
    ( updateModelWithUser maybeUser
        { screenName = ""
        , gameType = "Basic GIPF"
        , color = "white"
        , error = Nothing
        }
    , Cmd.none
    )


updateModelWithUser : Maybe Auth.User -> Model -> Model
updateModelWithUser maybeUser model =
    case maybeUser of
        Just user ->
            { model | screenName = user.screenName }

        Nothing ->
            { model | screenName = "" }


type Msg
    = SelectGameType String
    | SelectColor String
    | CreateGame
    | CreateGameReceived (HttpResult Game)
    | NoOp


type alias Game =
    { id : Int
    , gameType : String
    , whitePlayer : String
    , blackPlayer : String
    }


gameDecoder : Decode.Decoder Game
gameDecoder =
    Decode.map4 Game
        (Decode.field "id" Decode.int)
        (Decode.field "type" Decode.string)
        (Decode.field "white_player" Decode.string)
        (Decode.field "black_player" Decode.string)


createGame : String -> String -> String -> Cmd Msg
createGame gameType color screenName =
    let
        player =
            if color == "white" then
                "white_player"

            else
                "black_player"
    in
    Http.post
        { url = "/game/create"
        , body =
            Http.jsonBody <|
                Encode.object
                    [ ( "type", Encode.string gameType )
                    , ( player, Encode.string screenName )
                    ]
        , expect = Http.expectJson CreateGameReceived (responseDecoder gameDecoder)
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectGameType gameType ->
            ( { model | gameType = gameType }, Cmd.none )

        SelectColor color ->
            ( { model | color = color }, Cmd.none )

        CreateGame ->
            ( model, createGame model.gameType model.color model.screenName )

        CreateGameReceived result ->
            case parseResult result of
                Ok _ ->
                    ( { model | error = Nothing }, Cmd.none )

                Err error ->
                    ( { model | error = Just error }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


viewCreateNewGame : Model -> Html Msg
viewCreateNewGame model =
    if model.screenName == "" then
        div [] []

    else
        div [ class "p-4" ]
            [ h2 [ class "text-lg font-bold mb-4" ] [ text "Create new game" ]
            , div [ class "flex items-center space-x-4" ]
                [ label [ class "mr-2" ] [ text "Select game type:" ]
                , select [ class "form-select", onInput SelectGameType ]
                    [ option [] [ text "Basic GIPF" ]
                    , option [] [ text "Standard GIPF" ]
                    , option [] [ text "Tournament GIPF" ]
                    ]
                , label [ class "ml-4 mr-2" ] [ text "Play as:" ]
                , div [ class "flex items-center space-x-4" ]
                    [ label []
                        [ input
                            [ type_ "radio"
                            , name "choice"
                            , value "white"
                            , class "form-radio"
                            , checked True
                            , onCheck
                                (\isChecked ->
                                    if isChecked then
                                        SelectColor "white"

                                    else
                                        NoOp
                                )
                            ]
                            []
                        , text " White"
                        ]
                    , label []
                        [ input
                            [ type_ "radio"
                            , name "choice"
                            , value "black"
                            , class "form-radio"
                            , onCheck
                                (\isChecked ->
                                    if isChecked then
                                        SelectColor "black"

                                    else
                                        NoOp
                                )
                            ]
                            []
                        , text " Black"
                        ]
                    ]
                , viewPrimaryButton ( "Create game", CreateGame )
                ]
            , viewErrorMessage model.error
            ]
