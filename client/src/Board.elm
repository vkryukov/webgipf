module Board exposing (..)

import Browser
import Debug
import Html exposing (Html)
import Html.Events exposing (onMouseEnter, onMouseLeave, onMouseOver)
import Svg exposing (Svg, circle, g, line, polygon, rect, svg, text_)
import Svg.Attributes exposing (cx, cy, fill, height, mode, points, r, stroke, strokeWidth, viewBox, width, x, x1, x2, y, y1, y2)
import Svg.Events exposing (onClick)



-- Model definition


type alias Coord =
    { x : Int, y : Int }


type alias Piece =
    { coord : Coord, kind : Kind }


type Kind
    = Black
    | BlackGipf
    | White
    | WhiteGipf


type alias Move =
    { from : Coord, to : Coord }


type alias Model =
    { pieces : List Piece
    , availableMoves : List Move
    , currentColor : Kind
    , highlightedPiece : Maybe Coord
    , moveFrom : Maybe Coord
    , moveTo : Maybe Coord
    }


boardPointQ : Coord -> Bool
boardPointQ point =
    point.x >= 0 && point.x <= 8 && Basics.max (point.x - 4) 0 <= point.y && point.y <= Basics.min (4 + point.x) 8


interiorBoardPointQ : Coord -> Bool
interiorBoardPointQ point =
    point.x >= 1 && point.x <= 7 && point.y >= 1 && point.y <= 7 && abs (point.x - point.y) < 4


edgeBoardPointQ : Coord -> Bool
edgeBoardPointQ point =
    point.x == 0 || point.x == 8 || point.y == 0 || point.y == 8 || abs (point.x - point.y) == 4


boardPoints : List Coord
boardPoints =
    List.concatMap (\x -> List.map (\y -> { x = x, y = y }) (List.range 0 8)) (List.range 0 8)
        |> List.filter boardPointQ


interiorBoardPoints : List Coord
interiorBoardPoints =
    List.filter interiorBoardPointQ boardPoints


edgeBoardPoints : List Coord
edgeBoardPoints =
    List.filter edgeBoardPointQ boardPoints


init : Model
init =
    { pieces =
        [ Piece (Coord 4 1) BlackGipf
        , Piece (Coord 7 7) BlackGipf
        , Piece (Coord 1 4) BlackGipf
        , Piece (Coord 4 7) WhiteGipf
        , Piece (Coord 7 4) WhiteGipf
        , Piece (Coord 1 1) WhiteGipf
        ]
    , availableMoves =
        [ Move (Coord 0 3) (Coord 1 4)
        , Move (Coord 0 3) (Coord 1 3)
        ]
    , currentColor = Black
    , highlightedPiece = Nothing
    , moveFrom = Nothing
    , moveTo = Nothing
    }



-- Update


type Msg
    = ChangeColor
    | MouseEnter Coord
    | MouseLeave Coord
    | PointClicked Coord


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeColor ->
            { model
                | currentColor =
                    if model.currentColor == Black then
                        BlackGipf

                    else if model.currentColor == BlackGipf then
                        White

                    else if model.currentColor == White then
                        WhiteGipf

                    else
                        Black
            }

        MouseEnter coord ->
            { model | highlightedPiece = Just coord }

        MouseLeave _ ->
            { model | highlightedPiece = Nothing }

        PointClicked coord ->
            case model.moveFrom of
                Nothing ->
                    let
                        _ =
                            Debug.log "PointClicked" ("Move from " ++ Debug.toString coord)
                    in
                    { model | moveFrom = Just coord, highlightedPiece = Nothing }

                Just from ->
                    if from == coord then
                        { model | moveFrom = Nothing, highlightedPiece = Nothing }

                    else
                        let
                            _ =
                                Debug.log "PointClicked" ("Move from " ++ Debug.toString from ++ " to " ++ Debug.toString coord)
                        in
                        { model | moveTo = Just coord, highlightedPiece = Nothing }



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
coordToXY coord =
    ( round (toFloat coord.x * cos30 * scale) + offsetX, offsetY - round ((toFloat coord.y - sin30 * toFloat coord.x) * scale) )


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
                    [ Coord 1 1
                    , Coord 4 1
                    , Coord 7 4
                    , Coord 7 7
                    , Coord 4 7
                    , Coord 1 4
                    , Coord 1 1
                    ]
                )
            , Svg.Attributes.style "fill: white;"
            ]
            []

         -- lines
         , drawLine (Coord 1 0) (Coord 1 5)
         , drawLine (Coord 2 0) (Coord 2 6)
         , drawLine (Coord 3 0) (Coord 3 7)
         , drawLine (Coord 4 0) (Coord 4 8)
         , drawLine (Coord 5 1) (Coord 5 8)
         , drawLine (Coord 6 2) (Coord 6 8)
         , drawLine (Coord 7 3) (Coord 7 8)
         , drawLine (Coord 0 3) (Coord 5 8)
         , drawLine (Coord 0 2) (Coord 6 8)
         , drawLine (Coord 0 1) (Coord 7 8)
         , drawLine (Coord 0 0) (Coord 8 8)
         , drawLine (Coord 1 0) (Coord 8 7)
         , drawLine (Coord 2 0) (Coord 8 6)
         , drawLine (Coord 3 0) (Coord 8 5)
         , drawLine (Coord 3 7) (Coord 8 7)
         , drawLine (Coord 2 6) (Coord 8 6)
         , drawLine (Coord 1 5) (Coord 8 5)
         , drawLine (Coord 0 4) (Coord 8 4)
         , drawLine (Coord 0 3) (Coord 7 3)
         , drawLine (Coord 0 2) (Coord 6 2)
         , drawLine (Coord 0 1) (Coord 5 1)

         -- bottom labels
         , drawBottomLabel "a1" (Coord 0 0)
         , drawBottomLabel "b1" (Coord 1 0)
         , drawBottomLabel "c1" (Coord 2 0)
         , drawBottomLabel "d1" (Coord 3 0)
         , drawBottomLabel "e1" (Coord 4 0)
         , drawBottomLabel "f1" (Coord 5 1)
         , drawBottomLabel "g1" (Coord 6 2)
         , drawBottomLabel "h1" (Coord 7 3)
         , drawBottomLabel "i1" (Coord 8 4)

         -- top labels
         , drawTopLabel "a5" (Coord 0 4)
         , drawTopLabel "b6" (Coord 1 5)
         , drawTopLabel "c7" (Coord 2 6)
         , drawTopLabel "d8" (Coord 3 7)
         , drawTopLabel "e9" (Coord 4 8)
         , drawTopLabel "f8" (Coord 5 8)
         , drawTopLabel "g7" (Coord 6 8)
         , drawTopLabel "h6" (Coord 7 8)
         , drawTopLabel "i5" (Coord 8 8)
         ]
            ++ List.map (\p -> drawCircle p 0.1 "black") edgeBoardPoints
        )


viewPiece : Piece -> Svg msg
viewPiece { coord, kind } =
    case kind of
        Black ->
            drawCircle coord 0.25 "black"

        BlackGipf ->
            g []
                [ drawCircle coord 0.25 "black"
                , drawCircleWithStroke coord 0.125 "none" "white" "2"
                ]

        White ->
            drawCircle coord 0.25 "lightyellow"

        WhiteGipf ->
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
        (List.map viewPiece model.pieces)


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
        drawHighlightedPiece model.highlightedPiece model.currentColor

    else
        g []
            [ drawHighlightedPiece model.moveFrom model.currentColor
            , drawHighlightedPiece model.highlightedPiece model.currentColor
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


view : Model -> Html Msg
view model =
    svg
        [ width "660"
        , height "780"
        , viewBox "0 0 660 780"
        , Svg.Attributes.style "user-select: none; -webkit-user-select: none; -moz-user-select: none; -ms-user-select: none;"
        ]
        [ viewEmptyBoard
        , viewPieceWithAction (Piece (Coord 8 10) model.currentColor) "click" ChangeColor
        , viewPieces model
        , viewPossibleMoves model
        ]


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
