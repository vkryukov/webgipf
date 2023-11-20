module Board exposing (..)

import Browser
import Draw exposing (..)
import Gipf exposing (..)
import Html exposing (Html, button, div, form, input, p, s, text)
import Html.Attributes exposing (disabled, placeholder, style, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Platform.Cmd as Cmd
import Svg exposing (Svg, g, rect, svg)
import Svg.Attributes exposing (fill, fontSize, height, viewBox, width, x, y)
import Svg.Events exposing (onClick)
import Task



-- Model definition


type alias Model =
    { game : Game
    , kind : Kind
    , highlightedPiece : Maybe Coord
    , moveFrom : Maybe Coord
    , moveTo : Maybe Coord
    , move : Maybe Direction
    , possibleMoves : List Direction
    , selectedToRemove : List Coord
    , autoSelectedToRemove : List Coord
    , boardInput : String
    }


init : () -> ( Model, Cmd msg )
init =
    \_ ->
        -- initFromString "GWi3-h3 GKb6-c6 GWi2-h3 GKc7-c6 GWi2-h3 GKc7-c6 Wi4-h4 Ka5-b5 Wi3-h4 Ki4-h4 Wi3-h4 Kg7-g6 Wb6-c6 Kb6-c6 Wi3-h4 Kf8-f7 Wg1-f2 Ka5-b5"
        -- initFromGame standardGame
        -- initFromGame basicGame
        -- one move creates two groups
        -- initFromString "We1-e2 Ka1-b2 Wa5-b5 Ke9-e8 Wi5-h5 Ki1-h2 Wc7-c6 Ka3-b4 Wd8-d7 Ka3-b4 Wi4-h4 Ka3-b4"
        initFromString "GWi3-h3 GKb6-c6 GWi2-h3 GKc7-c6 GWi2-h3 GKc7-c6 Wi4-h4 Ka5-b5 Wi3-h4 Ki4-h4 Wi3-h4 Kg7-g6 Wb6-c6 Kb6-c6 Wi3-h4 Kf8-f7 Wg1-f2 Ka5-b5 xf6,g6 Wa4-b5 Ka4-b5 Wd8-d7 Ke9-e8 xc5,d6,e7,f7 xe6,g4 Wb6-c6 Ka5-b5 Wh6-h5 Ki4-h5 Wi4-h5 Kg7-g6 Wi3-h4 Kh6-h5 Wa5-b5 Ki3-h3 Wi4-h4"


initFromGame : Game -> ( Model, Cmd msg )
initFromGame game =
    ( { game = game
      , kind = game.currentKind
      , highlightedPiece = Nothing
      , moveFrom = Nothing
      , moveTo = Nothing
      , move = Nothing
      , possibleMoves = availableMoves game.board
      , selectedToRemove = []
      , autoSelectedToRemove = autoSelectToRemove game
      , boardInput = ""
      }
    , Cmd.none
    )


initFromString : String -> ( Model, Cmd msg )
initFromString s =
    initFromGame (stringToGameWithDefault s)



-- Update


type Msg
    = MouseEnter Coord
    | MouseLeave Coord
    | PointClicked Coord
    | MoveMade Direction
    | SaveBoardInput String
    | UpdateBoard
    | ChangeKind
    | RemovePieces


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                        , Task.succeed (MoveMade (Direction from coord)) |> Task.perform identity
                        )

        MoveMade move ->
            let
                g =
                    performMoveWithDefaultColor move model.kind model.game
            in
            case g of
                Just g1 ->
                    -- move was valid
                    ( { model
                        | game = g1
                        , kind = g1.currentKind
                        , possibleMoves = availableMoves g1.board
                        , autoSelectedToRemove = autoSelectToRemove g1
                      }
                    , Cmd.none
                    )

                Nothing ->
                    -- move was invalid
                    let
                        _ =
                            Debug.log "invalid move" move
                    in
                    ( model, Cmd.none )

        SaveBoardInput str ->
            ( { model | boardInput = str }, Cmd.none )

        UpdateBoard ->
            let
                ( m, _ ) =
                    initFromString model.boardInput

                _ =
                    Debug.log "m" m
            in
            ( { m | boardInput = "" }, Cmd.none )

        ChangeKind ->
            ( { model
                | kind =
                    if model.kind == Gipf then
                        Regular

                    else
                        Gipf
              }
            , Cmd.none
            )

        RemovePieces ->
            let
                g =
                    performAction (Just (RemoveAction model.autoSelectedToRemove)) (Just model.game)

                _ =
                    Debug.log "g" g
            in
            case g of
                Just g1 ->
                    ( { model
                        | game = g1
                        , possibleMoves = availableMoves g1.board
                        , kind = g1.currentKind
                        , autoSelectedToRemove = autoSelectToRemove g1
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )



-- View


viewEdgePoints : Svg msg
viewEdgePoints =
    g []
        (List.map drawEdgePoint edgeBoardPoints)


viewEmptyBoard : Svg msg
viewEmptyBoard =
    g []
        [ -- interior polygon
          rect
            [ x "25"
            , y "25"
            , width "610"
            , height "730"
            , fill "#F0F0F0"
            ]
            []
        , drawPolygon
            [ ( 1, 1 )
            , ( 4, 1 )
            , ( 7, 4 )
            , ( 7, 7 )
            , ( 4, 7 )
            , ( 1, 4 )
            , ( 1, 1 )
            ]
            "white"

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

        -- edge points
        , viewEdgePoints
        ]


viewPieces : Model -> Svg msg
viewPieces model =
    g []
        (List.map drawPiece (boardToPieces model.game.board))


viewPossibleMoves : Model -> Svg Msg
viewPossibleMoves model =
    case model.game.state of
        WaitingForMove ->
            let
                possibleClicks =
                    case model.moveFrom of
                        Nothing ->
                            List.map .from model.possibleMoves

                        Just coord ->
                            if model.moveTo == Nothing then
                                coord :: List.map .to (List.filter (\move -> move.from == coord) model.possibleMoves)

                            else
                                []
            in
            g []
                (viewHighlights model
                    :: List.map (\p -> drawClickPoint p MouseEnter MouseLeave PointClicked) possibleClicks
                )

        _ ->
            g [] []


viewHighlights : Model -> Svg msg
viewHighlights model =
    if model.moveFrom == Nothing then
        drawHighlightedPiece model.highlightedPiece model.kind model.game.currentColor

    else
        g []
            [ drawHighlightedPiece model.moveFrom model.kind model.game.currentColor
            , drawHighlightedPiece model.highlightedPiece model.kind model.game.currentColor
            ]


viewMove : Model -> Svg msg
viewMove model =
    case model.move of
        Just move ->
            drawArrow move.from move.to

        Nothing ->
            g [] []


viewConnectedPieces : Model -> Svg msg
viewConnectedPieces model =
    let
        allStones =
            model.game.currentPlayerFourStones ++ model.game.otherPlayerFourStones
    in
    g []
        (List.map
            (\group ->
                g []
                    (List.map (\p -> drawLightMark p.coord) group)
            )
            allStones
            ++ [ g [] (List.map drawDarkMark model.autoSelectedToRemove) ]
        )


viewCurrentAction : Model -> Svg Msg
viewCurrentAction model =
    if model.game.state == WaitingForMove then
        if
            (model.game.currentKind == Regular)
                || (not model.game.isBasicGame
                        -- In the tournament, you have to play at least one Gipf piece
                        && ((model.game.currentColor == Black && model.game.blackGipfCount == 0)
                                || (model.game.currentColor == White && model.game.whiteGipfCount == 0)
                           )
                   )
        then
            drawPiece (Piece ( 8, 10 ) model.game.currentColor model.game.currentKind)

        else
            let
                pieceLabel =
                    if model.kind == Regular then
                        "Gipf"

                    else
                        "Regular"
            in
            g []
                [ drawPieceWithAction (Piece ( 8, 10 ) model.game.currentColor model.kind) "click" ChangeKind
                , drawMultilineTextAtCoord ("Click to\nchange\nto " ++ pieceLabel) ( 8, 10 ) -25 35 10
                ]

    else if model.game.state == WaitingForRemove then
        g []
            [ drawPiece
                (Piece ( 8, 10 )
                    (if model.game.currentPlayerFourStones == [] then
                        reverseColor model.game.currentColor

                     else
                        model.game.currentColor
                    )
                    Regular
                )
            , drawDarkMark ( 8, 10 )
            , if model.autoSelectedToRemove == [] then
                drawMultilineTextAtCoord "Click on a\ngroup to\nremove first" ( 8, 10 ) -35 35 12

              else
                g [] []
            ]

    else
        -- game is over
        g [] []


viewPiecesCounts : Model -> Svg msg
viewPiecesCounts model =
    g []
        [ drawPiece (Piece ( 0, -2 ) White Regular)
        , drawPiece (Piece ( 8, 2 ) Black Regular)
        , drawMultilineTextAtCoord
            ("Remaining: " ++ String.fromInt model.game.whiteCount.own ++ "\nCaptured: " ++ String.fromInt model.game.whiteCount.captured)
            ( 0, -2 )
            25
            0
            13
        , drawMultilineTextAtCoord
            ("Remaining: " ++ String.fromInt model.game.blackCount.own ++ "\nCaptured: " ++ String.fromInt model.game.blackCount.captured)
            ( 8, 2 )
            -90
            0
            13
        ]


viewMultiGroupSelector : Model -> Svg Msg
viewMultiGroupSelector model =
    if model.game.state == WaitingForRemove && model.autoSelectedToRemove == [] then
        g []
            (List.map
                (\group ->
                    g []
                        (List.map (\p -> drawLightMark p.coord) group)
                )
                model.game.currentPlayerFourStones
            )

    else
        g [] []


viewConfirmRemoveButton : Model -> Html Msg
viewConfirmRemoveButton model =
    if model.autoSelectedToRemove == [] then
        div [] []

    else
        button
            [ style "position" "absolute"
            , style "top" "100px"
            , style "left" "570px"
            , onClick RemovePieces
            ]
            [ text "Remove" ]


view : Model -> Html Msg
view model =
    div [ style "position" "relative" ]
        [ svg
            [ width "660"
            , height "780"
            , viewBox "0 0 660 780"
            , Svg.Attributes.style "user-select: none; -webkit-user-select: none; -moz-user-select: none; -ms-user-select: none;"
            ]
            [ viewEmptyBoard
            , viewCurrentAction model
            , viewPieces model
            , viewConnectedPieces model
            , viewPossibleMoves model
            , viewPiecesCounts model
            , viewMultiGroupSelector model
            , viewMove model
            ]
        , viewConfirmRemoveButton model
        , div
            [ style "text-align" "left"
            , style "padding-left" "25px"
            , style "width" "610px"
            , style "word-wrap" "break-word"
            ]
            [ p [] [ text (actionsToString model.game.actionHistory) ]
            , p [ fontSize "6" ] [ text (Debug.toString model.game) ]
            , p
                []
                [ form
                    [ onSubmit UpdateBoard
                    ]
                    [ div []
                        [ input
                            [ type_ "text"
                            , placeholder "Enter new game..."
                            , value model.boardInput
                            , onInput SaveBoardInput
                            , style "width" "520px"
                            ]
                            []
                        , button [ disabled (String.isEmpty model.boardInput) ] [ text "Update" ]
                        ]
                    ]
                ]
            ]
        ]


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }
