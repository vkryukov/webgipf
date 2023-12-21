module ServerUtils exposing
    ( HttpResult
    , parseResult
    , responseDecoder
    )

import Http
import Json.Decode as Decode


type Response a
    = OkResponse a
    | ErrorResponse String


responseDecoder : Decode.Decoder a -> Decode.Decoder (Response a)
responseDecoder decoder =
    Decode.oneOf
        [ Decode.map OkResponse decoder
        , Decode.map ErrorResponse (Decode.field "error" Decode.string)
        ]


type alias HttpResult a =
    Result Http.Error (Response a)


parseResult : HttpResult a -> Result String a
parseResult result =
    case result of
        Ok (OkResponse value) ->
            Ok value

        Ok (ErrorResponse message) ->
            Err message

        Err error ->
            Err (httpErrorToString error)


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Request timed out"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus statusCode ->
            "Bad status: " ++ String.fromInt statusCode

        Http.BadBody message ->
            "Bad body: " ++ message
