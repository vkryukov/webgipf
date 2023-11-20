module Draw exposing
    ( drawArrow
    , drawBottomLabel
    , drawCircle
    , drawClickPoint
    , drawDarkMark
    , drawEdgePoint
    , drawHighlightedPiece
    , drawLightMark
    , drawLine
    , drawMultilineTextAtCoord
    , drawPiece
    , drawPieceWithAction
    , drawPolygon
    , drawTopLabel
    )

-- Draw contains a set of drawing primitives that can be used to draw SVG objects on the screen.
-- They are support Coord and translate to SVG coordinates.

import Gipf exposing (Color(..), Coord, Kind(..), Piece)
import Html.Events exposing (onMouseEnter, onMouseLeave, onMouseOver)
import Svg exposing (Svg, circle, g, line, polygon, text_)
import Svg.Attributes exposing (cx, cy, fill, fontSize, points, r, stroke, strokeWidth, x, x1, x2, y, y1, y2)
import Svg.Events exposing (onClick)



-- Translations from Coord to SVG coordinates.


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



-- Drawing primitives.


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


drawPolygon : List Coord -> String -> Svg msg
drawPolygon coords fill_ =
    let
        points_ =
            coordsToPoints coords
    in
    polygon [ fill fill_, points points_ ] []


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



-- Composite drawing primitives.


drawBottomLabel : String -> Coord -> Svg msg
drawBottomLabel label p =
    drawTextLabel label p -5 25


drawTopLabel : String -> Coord -> Svg msg
drawTopLabel label p =
    drawTextLabel label p -5 -15


markRadius : Float
markRadius =
    0.08


drawLightMark : Coord -> Svg msg
drawLightMark coord =
    drawCircle coord markRadius "LightCoral"


drawDarkMark : Coord -> Svg msg
drawDarkMark coord =
    drawCircle coord markRadius "Red"


pieceRadius : Float
pieceRadius =
    0.25


innerPieceRadius : Float
innerPieceRadius =
    0.125


drawPiece : Piece -> Svg msg
drawPiece { coord, color, kind } =
    if kind == Regular && color == Black then
        drawCircle coord pieceRadius "black"

    else if kind == Gipf && color == Black then
        g []
            [ drawCircle coord pieceRadius "black"
            , drawCircleWithStroke coord innerPieceRadius "none" "white" "2"
            ]

    else if kind == Regular && color == White then
        drawCircle coord pieceRadius "lightyellow"

    else
        g []
            [ drawCircle coord pieceRadius "lightyellow"
            , drawCircleWithStroke coord innerPieceRadius "none" "black" "2"
            ]


drawEdgePoint : Coord -> Svg msg
drawEdgePoint coord =
    drawCircle coord 0.1 "black"



-- Primitives with interactivity.


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
                [ drawCircleWithStroke coord pieceRadius "white" "grey" "1"
                , addSvgOpacity (drawPiece (Piece coord color kind)) 0.5
                ]

        Nothing ->
            g [] []


drawPieceWithAction : Piece -> String -> msg -> Svg msg
drawPieceWithAction piece event msg =
    addSvgAction (drawPiece piece) event msg


drawClickPoint : Coord -> (Coord -> msg) -> (Coord -> msg) -> (Coord -> msg) -> Svg msg
drawClickPoint p mouseEnter mouseLeave pointClicked =
    let
        ( x, y ) =
            coordToXY p
    in
    circle
        [ cx (String.fromInt x)
        , cy (String.fromInt y)
        , r (String.fromInt (round (innerPieceRadius * scale)))
        , fill "none"
        , onMouseEnter (mouseEnter p)
        , onMouseLeave (mouseLeave p)
        , onClick (pointClicked p)

        -- This is needed to make the circle clickable, as transparent SVG elements are not clickable by default.
        , Svg.Attributes.style "pointer-events: all;"
        ]
        []
