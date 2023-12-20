module ServerUtils exposing (Response(..), responseDecoder)

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
