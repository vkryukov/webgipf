module Routes exposing (Route(..), href, match)

import Html
import Html.Attributes
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


type Route
    = Home
    | SignIn
    | SignUp
    | SignOut
    | PlayGame Int
    | ViewGame Int


routes : Parser (Route -> a) a
routes =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map SignIn (Parser.s "signin")
        , Parser.map SignUp (Parser.s "signup")
        , Parser.map SignOut (Parser.s "signout")
        , Parser.map PlayGame (Parser.s "play" </> Parser.int)
        , Parser.map ViewGame (Parser.s "view" </> Parser.int)
        ]


match : Url -> Maybe Route
match url =
    Parser.parse routes url


routeToUrl : Route -> String
routeToUrl route =
    case route of
        Home ->
            "/"

        SignIn ->
            "/signin"

        SignUp ->
            "/signup"

        SignOut ->
            "/signout"

        PlayGame id ->
            "/play/" ++ String.fromInt id

        ViewGame id ->
            "/view/" ++ String.fromInt id


href : Route -> Html.Attribute msg
href route =
    Html.Attributes.href (routeToUrl route)
