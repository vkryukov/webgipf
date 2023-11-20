module Draw exposing (..)

import Gipf exposing (Color(..), Coord, Kind(..), Piece)
import Html.Events exposing (onMouseEnter, onMouseLeave, onMouseOver)
import Svg exposing (Svg, circle, g, line, text_)
import Svg.Attributes exposing (cx, cy, fill, fontSize, r, stroke, strokeWidth, x, x1, x2, y, y1, y2)
import Svg.Events exposing (onClick)



-- Draw contains a set of drawing primitives that can be used to draw SVG objects on the screen.
-- They are support Coord and translate to SVG coordinates.


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


drawClickPoint : Coord -> Float -> (Coord -> msg) -> (Coord -> msg) -> (Coord -> msg) -> Svg msg
drawClickPoint p radius mouseEnter mouseLeave pointClicked =
    let
        ( x, y ) =
            coordToXY p
    in
    circle
        [ cx (String.fromInt x)
        , cy (String.fromInt y)
        , r (String.fromInt (round (radius * scale)))
        , fill "none"
        , onMouseEnter (mouseEnter p)
        , onMouseLeave (mouseLeave p)
        , onClick (pointClicked p)
        , Svg.Attributes.style "pointer-events: all;"
        ]
        []


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


drawLightMark : Coord -> Svg msg
drawLightMark coord =
    drawCircle coord 0.09 "LightCoral"


drawDarkMark : Coord -> Svg msg
drawDarkMark coord =
    drawCircle coord 0.09 "Red"


drawMultilineText : String -> ( Int, Int ) -> Int -> Int -> Int -> Svg msg
drawMultilineText text ( x_, y_ ) dx dy offY =
    let
        lines =
            String.split "\n" text
    in
    g []
        (List.indexedMap
            (\i line ->
                text_
                    [ x (String.fromInt (x_ + dx))
                    , y (String.fromInt (y_ + i * offY + dy))
                    , fontSize "12"
                    ]
                    [ Svg.text line ]
            )
            lines
        )


drawMultilineTextAtCoord : String -> Coord -> Int -> Int -> Int -> Svg msg
drawMultilineTextAtCoord text coord dx dy offY =
    let
        ( x_, y_ ) =
            coordToXY coord
    in
    drawMultilineText text ( x_, y_ ) dx dy offY


drawPiece : Piece -> Svg msg
drawPiece { coord, color, kind } =
    if kind == Regular && color == Black then
        drawCircle coord 0.25 "black"

    else if kind == Gipf && color == Black then
        g []
            [ drawCircle coord 0.25 "black"
            , drawCircleWithStroke coord 0.125 "none" "white" "2"
            ]

    else if kind == Regular && color == White then
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


drawHighlightedPiece : Maybe Coord -> Kind -> Color -> Svg msg
drawHighlightedPiece maybeCoord kind color =
    case maybeCoord of
        Just coord ->
            g []
                [ drawCircleWithStroke coord 0.25 "white" "grey" "1"
                , addSvgOpacity (drawPiece (Piece coord color kind)) 0.5
                ]

        Nothing ->
            g [] []
