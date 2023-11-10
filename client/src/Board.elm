module Board exposing (..)

import Browser
import Gipf exposing (..)
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onMouseEnter, onMouseLeave, onMouseOver)
import Platform.Cmd as Cmd
import Svg exposing (Svg, circle, g, line, polygon, rect, svg, text_)
import Svg.Attributes exposing (cx, cy, fill, height, points, r, stroke, strokeWidth, viewBox, width, x, x1, x2, y, y1, y2)
import Svg.Events exposing (onClick)
import Task



-- Model definition


type alias Model =
    { board : BoardPieces
    , availableMoves : List Move
    , groupsOfFour : List (List Piece)
    , currentKind : Kind
    , highlightedPiece : Maybe Coord
    , moveFrom : Maybe Coord
    , moveTo : Maybe Coord
    , move : Maybe Move
    }


init : () -> ( Model, Cmd msg )
init =
    \_ ->
        initFromString "GKb5 GKe2 GKh5 GWb2 GWe5 GWe8 Kd4 Ke4 Kf3 Kg3 Wf4 Wg2 Wg4 Wh2 Wh3"


initFromString : String -> ( Model, Cmd msg )
initFromString s =
    ( initFromBoard (Maybe.withDefault standardStartingBoard (stringToBoard s))
    , Cmd.none
    )


initFromBoard : BoardPieces -> Model
initFromBoard b =
    { board = b
    , availableMoves = availableMoves b
    , groupsOfFour = connectedGroupsOfFour b
    , currentKind = Kind White Regular
    , highlightedPiece = Nothing
    , moveFrom = Nothing
    , moveTo = Nothing
    , move = Nothing
    }



-- Update


type Msg
    = ChangeColor
    | MouseEnter Coord
    | MouseLeave Coord
    | PointClicked Coord
    | MoveMade Move


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeColor ->
            ( { model
                | currentKind =
                    if model.currentKind == Kind Black Regular then
                        Kind Black Gipf

                    else if model.currentKind == Kind Black Gipf then
                        Kind White Regular

                    else if model.currentKind == Kind White Regular then
                        Kind White Gipf

                    else
                        Kind Black Regular
              }
            , Cmd.none
            )

        MouseEnter coord ->
            ( { model | highlightedPiece = Just coord }, Cmd.none )

        MouseLeave _ ->
            ( { model | highlightedPiece = Nothing }, Cmd.none )

        PointClicked coord ->
            case model.moveFrom of
                Nothing ->
                    ( { model | moveFrom = Just coord, highlightedPiece = Nothing }, Cmd.none )

                Just from ->
                    if from == coord then
                        ( { model | moveFrom = Nothing, highlightedPiece = Nothing }, Cmd.none )

                    else
                        ( { model
                            | moveFrom = Nothing
                            , moveTo = Nothing
                            , highlightedPiece = Nothing
                          }
                        , Task.succeed (MoveMade (Move from coord)) |> Task.perform identity
                        )

        MoveMade move ->
            let
                b =
                    performMove move model.currentKind model.board
            in
            case b of
                Just b1 ->
                    -- move was valid
                    ( { model
                        | board = b1
                        , availableMoves = availableMoves b1
                        , groupsOfFour = connectedGroupsOfFour b1
                        , currentKind =
                            Kind
                                (if model.currentKind.color == White then
                                    Black

                                 else
                                    White
                                )
                                model.currentKind.kind
                      }
                    , Cmd.none
                    )

                Nothing ->
                    -- move was invalid
                    ( model, Cmd.none )



-- View


cos30 : Float
cos30 =
    cos (pi / 6)


sin30 : Float
sin30 =
    sin (pi / 6)


offsetX : Int
offsetX =
    50


offsetY : Int
offsetY =
    550


scale : Float
scale =
    80


coordToXY : Coord -> ( Int, Int )
coordToXY ( x, y ) =
    ( round (toFloat x * cos30 * scale) + offsetX, offsetY - round ((toFloat y - sin30 * toFloat x) * scale) )


coordsToPoints : List Coord -> String
coordsToPoints coords =
    -- coordsToPoints converts a list of coordinates to a string of points used in the SVG polygon
    List.map coordToXY coords
        |> List.map (\( x, y ) -> String.fromInt x ++ "," ++ String.fromInt y)
        |> String.join " "


drawLine : Coord -> Coord -> Svg msg
drawLine p1 p2 =
    let
        ( x1_, y1_ ) =
            coordToXY p1

        ( x2_, y2_ ) =
            coordToXY p2
    in
    line
        [ x1 (String.fromInt x1_)
        , y1 (String.fromInt y1_)
        , x2 (String.fromInt x2_)
        , y2 (String.fromInt y2_)
        , stroke "black"
        ]
        []


drawCircleWithStroke : Coord -> Float -> String -> String -> String -> Svg msg
drawCircleWithStroke p radius fill_ stroke_ strokeWidth_ =
    let
        ( x, y ) =
            coordToXY p
    in
    circle
        [ cx (String.fromInt x)
        , cy (String.fromInt y)
        , r (String.fromInt (round (radius * scale)))
        , fill fill_
        , stroke stroke_
        , strokeWidth strokeWidth_
        ]
        []


drawCircle : Coord -> Float -> String -> Svg msg
drawCircle p radius fill_ =
    drawCircleWithStroke p radius fill_ "black" "1"


drawTextLabel : String -> Coord -> Int -> Int -> Svg msg
drawTextLabel label p offX offY =
    let
        ( x1, y1 ) =
            coordToXY p
    in
    text_
        [ x (String.fromInt (x1 + offX))
        , y (String.fromInt (y1 + offY))
        , fill "black"
        ]
        [ Svg.text label ]


drawBottomLabel : String -> Coord -> Svg msg
drawBottomLabel label p =
    drawTextLabel label p -5 25


drawTopLabel : String -> Coord -> Svg msg
drawTopLabel label p =
    drawTextLabel label p -5 -15


drawClickPoint : Coord -> Float -> Svg Msg
drawClickPoint p radius =
    let
        ( x, y ) =
            coordToXY p
    in
    circle
        [ cx (String.fromInt x)
        , cy (String.fromInt y)
        , r (String.fromInt (round (radius * scale)))
        , fill "none"
        , onMouseEnter (MouseEnter p)
        , onMouseLeave (MouseLeave p)
        , onClick (PointClicked p)
        , Svg.Attributes.style "pointer-events: all;"
        ]
        []


viewEmptyBoard : Svg msg
viewEmptyBoard =
    g []
        ([ -- interior polygon
           rect
            [ x "25"
            , y "25"
            , width "610"
            , height "730"
            , fill "#F0F0F0"
            ]
            []
         , polygon
            [ points
                (coordsToPoints
                    [ ( 1, 1 )
                    , ( 4, 1 )
                    , ( 7, 4 )
                    , ( 7, 7 )
                    , ( 4, 7 )
                    , ( 1, 4 )
                    , ( 1, 1 )
                    ]
                )
            , Svg.Attributes.style "fill: white;"
            ]
            []

         -- lines
         , drawLine ( 1, 0 ) ( 1, 5 )
         , drawLine ( 2, 0 ) ( 2, 6 )
         , drawLine ( 3, 0 ) ( 3, 7 )
         , drawLine ( 4, 0 ) ( 4, 8 )
         , drawLine ( 5, 1 ) ( 5, 8 )
         , drawLine ( 6, 2 ) ( 6, 8 )
         , drawLine ( 7, 3 ) ( 7, 8 )
         , drawLine ( 0, 3 ) ( 5, 8 )
         , drawLine ( 0, 2 ) ( 6, 8 )
         , drawLine ( 0, 1 ) ( 7, 8 )
         , drawLine ( 0, 0 ) ( 8, 8 )
         , drawLine ( 1, 0 ) ( 8, 7 )
         , drawLine ( 2, 0 ) ( 8, 6 )
         , drawLine ( 3, 0 ) ( 8, 5 )
         , drawLine ( 3, 7 ) ( 8, 7 )
         , drawLine ( 2, 6 ) ( 8, 6 )
         , drawLine ( 1, 5 ) ( 8, 5 )
         , drawLine ( 0, 4 ) ( 8, 4 )
         , drawLine ( 0, 3 ) ( 7, 3 )
         , drawLine ( 0, 2 ) ( 6, 2 )
         , drawLine ( 0, 1 ) ( 5, 1 )

         -- bottom labels
         , drawBottomLabel "a1" ( 0, 0 )
         , drawBottomLabel "b1" ( 1, 0 )
         , drawBottomLabel "c1" ( 2, 0 )
         , drawBottomLabel "d1" ( 3, 0 )
         , drawBottomLabel "e1" ( 4, 0 )
         , drawBottomLabel "f1" ( 5, 1 )
         , drawBottomLabel "g1" ( 6, 2 )
         , drawBottomLabel "h1" ( 7, 3 )
         , drawBottomLabel "i1" ( 8, 4 )

         -- top labels
         , drawTopLabel "a5" ( 0, 4 )
         , drawTopLabel "b6" ( 1, 5 )
         , drawTopLabel "c7" ( 2, 6 )
         , drawTopLabel "d8" ( 3, 7 )
         , drawTopLabel "e9" ( 4, 8 )
         , drawTopLabel "f8" ( 5, 8 )
         , drawTopLabel "g7" ( 6, 8 )
         , drawTopLabel "h6" ( 7, 8 )
         , drawTopLabel "i5" ( 8, 8 )
         ]
            ++ List.map (\p -> drawCircle p 0.1 "black") edgeBoardPoints
        )


viewPiece : Piece -> Svg msg
viewPiece { coord, kind } =
    if kind == Kind Black Regular then
        drawCircle coord 0.25 "black"

    else if kind == Kind Black Gipf then
        g []
            [ drawCircle coord 0.25 "black"
            , drawCircleWithStroke coord 0.125 "none" "white" "2"
            ]

    else if kind == Kind White Regular then
        drawCircle coord 0.25 "lightyellow"

    else
        g []
            [ drawCircle coord 0.25 "lightyellow"
            , drawCircleWithStroke coord 0.125 "none" "black" "2"
            ]


addSvgAction : Svg msg -> String -> msg -> Svg msg
addSvgAction svgElement event msg =
    case event of
        "hover" ->
            g [ onMouseOver msg ] [ svgElement ]

        "click" ->
            g [ onClick msg ] [ svgElement ]

        _ ->
            svgElement


addSvgOpacity : Svg msg -> Float -> Svg msg
addSvgOpacity originalSvg opacity =
    g [ Svg.Attributes.opacity (String.fromFloat opacity) ] [ originalSvg ]


viewPieceWithAction : Piece -> String -> msg -> Svg msg
viewPieceWithAction piece event msg =
    addSvgAction (viewPiece piece) event msg


viewPieces : Model -> Svg msg
viewPieces model =
    g []
        (List.map viewPiece (boardToPieces model.board))


viewPossibleMoves : Model -> Svg Msg
viewPossibleMoves model =
    let
        possibleClicks =
            case model.moveFrom of
                Nothing ->
                    List.map .from model.availableMoves

                Just coord ->
                    if model.moveTo == Nothing then
                        coord :: List.map .to (List.filter (\move -> move.from == coord) model.availableMoves)

                    else
                        []
    in
    g []
        (drawHighlights model
            :: List.map (\p -> drawClickPoint p 0.25) possibleClicks
        )


drawHighlights : Model -> Svg msg
drawHighlights model =
    if model.moveFrom == Nothing then
        drawHighlightedPiece model.highlightedPiece model.currentKind

    else
        g []
            [ drawHighlightedPiece model.moveFrom model.currentKind
            , drawHighlightedPiece model.highlightedPiece model.currentKind
            ]


drawHighlightedPiece : Maybe Coord -> Kind -> Svg msg
drawHighlightedPiece maybeCoord color =
    case maybeCoord of
        Just coord ->
            g []
                [ drawCircleWithStroke coord 0.25 "white" "grey" "1"
                , addSvgOpacity (viewPiece (Piece coord color)) 0.5
                ]

        Nothing ->
            g [] []


viewMove : Model -> Svg msg
viewMove model =
    case model.move of
        Just move ->
            drawArrow move.from move.to

        Nothing ->
            g [] []


drawArrow : Coord -> Coord -> Svg msg
drawArrow from to =
    let
        ( x1, y1 ) =
            coordToXY from

        ( x2, y2 ) =
            coordToXY to
    in
    drawArrowXY (toFloat x1) (toFloat y1) (toFloat x2) (toFloat y2)


degToRad : Float -> Float
degToRad deg =
    deg * pi / 180


drawArrowXY : Float -> Float -> Float -> Float -> Svg msg
drawArrowXY x1_ y1_ x2_ y2_ =
    let
        angle =
            atan2 (y2_ - y1_) (x2_ - x1_)

        arrowheadLength =
            15

        arrowheadAngle =
            degToRad 30

        arrowheadX1 =
            x2_ - arrowheadLength * cos (angle - arrowheadAngle)

        arrowheadY1 =
            y2_ - arrowheadLength * sin (angle - arrowheadAngle)

        arrowheadX2 =
            x2_ - arrowheadLength * cos (angle + arrowheadAngle)

        arrowheadY2 =
            y2_ - arrowheadLength * sin (angle + arrowheadAngle)
    in
    g []
        [ line [ x1 (String.fromFloat x1_), y1 (String.fromFloat y1_), x2 (String.fromFloat x2_), y2 (String.fromFloat y2_), stroke "red", strokeWidth "2" ] []
        , line [ x1 (String.fromFloat x2_), y1 (String.fromFloat y2_), x2 (String.fromFloat arrowheadX1), y2 (String.fromFloat arrowheadY1), stroke "red", strokeWidth "2" ] []
        , line [ x1 (String.fromFloat x2_), y1 (String.fromFloat y2_), x2 (String.fromFloat arrowheadX2), y2 (String.fromFloat arrowheadY2), stroke "red", strokeWidth "2" ] []
        ]


viewConnectedPieces : Model -> Svg msg
viewConnectedPieces model =
    g []
        (List.map
            (\group ->
                g []
                    (List.map
                        (\piece ->
                            drawCircle piece.coord 0.1 "LightCoral"
                        )
                        group
                    )
            )
            model.groupsOfFour
        )


view : Model -> Html Msg
view model =
    div []
        [ svg
            [ width "660"
            , height "780"
            , viewBox "0 0 660 780"
            , Svg.Attributes.style "user-select: none; -webkit-user-select: none; -moz-user-select: none; -ms-user-select: none;"
            ]
            [ viewEmptyBoard
            , viewPieceWithAction (Piece ( 8, 10 ) model.currentKind) "click" ChangeColor
            , viewPieces model
            , viewConnectedPieces model
            , viewPossibleMoves model
            , viewMove model
            ]
        , div
            [ style "text-align" "left"
            , style "padding-left" "25px"
            , style "width" "610px" -- adjust this value to match the width of the interior polygon
            , style "word-wrap" "break-word"
            ]
            [ p [] [ text (boardToString model.board) ] ]
        ]


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }
