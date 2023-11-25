module GipfBoard exposing (Model, Msg, initFromGame, initFromString, update, view)

import Browser
import Draw exposing (..)
import Gipf exposing (..)
import Html exposing (Html, button, div, p, s, text)
import Html.Attributes exposing (style)
import Platform.Cmd as Cmd
import Svg exposing (Svg, g, rect, svg)
import Svg.Attributes exposing (fill, fontSize, height, viewBox, width, x, y)
import Svg.Events exposing (onClick)
import Task
import Tools exposing (fst, snd)



-- Model definition


type alias Model =
    { game : Game

    -- move related
    , kind : Kind
    , highlightedPiece : Maybe Coord
    , moveFrom : Maybe Coord
    , moveTo : Maybe Coord
    , possibleMoves : List Direction

    -- remove related
    , autoSelected : ( List Coord, List Coord )
    , selectedToDisambiguate : Maybe Coord
    , gipfsSelected : List Coord
    , gipfHovered : Maybe Coord
    , allowActions : Bool
    , showDebug : Bool
    }


init : () -> ( Model, Cmd msg )
init =
    \_ ->
        {-
           initFromString "GWi3-h3 GKb6-c6 GWi2-h3 GKc7-c6 GWi2-h3 GKc7-c6 Wi4-h4 Ka5-b5 Wi3-h4 Ki4-h4 Wi3-h4 Kg7-g6 Wb6-c6 Kb6-c6 Wi3-h4 Kf8-f7 Wg1-f2 Ka5-b5"
           initFromGame standardGame
           initFromGame basicGame
           one move creates two groups
           initFromString "We1-e2 Ka1-b2 Wa5-b5 Ke9-e8 Wi5-h5 Ki1-h2 Wc7-c6 Ka3-b4 Wd8-d7 Ka3-b4 Wi4-h4 Ka3-b4"
           initFromString "GWi3-h3 GKb6-c6 GWi2-h3 GKc7-c6 GWi2-h3 GKc7-c6 Wi4-h4 Ka5-b5 Wi3-h4 Ki4-h4 Wi3-h4 Kg7-g6 Wb6-c6 Kb6-c6 Wi3-h4 Kf8-f7 Wg1-f2 Ka5-b5 xf6,g6 Wa4-b5 Ka4-b5 Wd8-d7 Ke9-e8 xc5,d6,e7,f7 xe6,g4 Wb6-c6 Ka5-b5 Wh6-h5 Ki4-h5 Wi4-h5 Kg7-g6 Wi3-h4 Kh6-h5 Wa5-b5 Ki3-h3 Wi4-h4"
        -}
        initFromGame standardGame


initFromGame : Game -> ( Model, Cmd msg )
initFromGame game =
    ( { game = game
      , kind = game.currentKind
      , highlightedPiece = Nothing
      , moveFrom = Nothing
      , moveTo = Nothing
      , possibleMoves = availableMoves game.board
      , autoSelected = autoSelectToRemove game
      , selectedToDisambiguate = Nothing
      , gipfsSelected = []
      , gipfHovered = Nothing -- TODO: Do we need this? We don't use it to draw anything.
      , allowActions = True
      , showDebug = True
      }
    , Cmd.none
    )


initFromString : String -> ( Model, Cmd msg )
initFromString s =
    initFromGame (stringToGameWithDefault s)


selected : Model -> List Coord
selected model =
    fst model.autoSelected ++ model.gipfsSelected



-- Update


type Msg
    = MouseEnter Coord
    | MouseLeave Coord
    | PointClicked Coord
    | MoveMade Direction
    | ChangeKind
    | RemovePieces
    | CancelRemovePieces
    | RemovalDisambiguationEnter Coord
    | RemovalDisambiguationLeave Coord
    | RemovalDisambiguationClick Coord
    | GipfEnter Coord
    | GipfLeave Coord
    | GipfClicked Coord


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
                    initFromGame g1

                Nothing ->
                    -- move was invalid
                    ( model, Cmd.none )

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
                    performAction (Just (RemoveAction (selected model))) (Just model.game)
            in
            case g of
                Just g1 ->
                    initFromGame g1

                Nothing ->
                    ( model, Cmd.none )

        CancelRemovePieces ->
            ( { model
                | autoSelected = ( [], [] )
              }
            , Cmd.none
            )

        RemovalDisambiguationEnter coord ->
            ( { model | selectedToDisambiguate = Just coord }, Cmd.none )

        RemovalDisambiguationLeave _ ->
            ( { model | selectedToDisambiguate = Nothing }, Cmd.none )

        RemovalDisambiguationClick coord ->
            ( { model | autoSelected = autoSelectToRemoveWithDisambiguation model.game coord }, Cmd.none )

        GipfEnter coord ->
            ( { model | gipfHovered = Just coord }, Cmd.none )

        GipfLeave _ ->
            ( { model | gipfHovered = Nothing }, Cmd.none )

        GipfClicked coord ->
            if List.member coord model.gipfsSelected then
                ( { model | gipfsSelected = List.filter ((/=) coord) model.gipfsSelected }, Cmd.none )

            else
                ( { model | gipfsSelected = coord :: model.gipfsSelected }, Cmd.none )



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
            [ x "0"
            , y "0"
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
            -95
            0
            13
        ]


{-|

    currentSelectionState determines the current state of the selection process.
    Basically, there are the following states:
        1. We are not removing any pieces - the simplest case.
        2. There is only one line that needs to be removed, with no Gipf pieces - we highlight it and wait for the player to confirm.
        3. There are few possible lines that can be removed, and we need to disambiguate between them.
        4. We selected a line to remove, and now we need to select the Gipf pieces to remove (this can also happen in case 2 when we autoselected the pieces).

-}
currentSelectionState : Model -> SelectionState
currentSelectionState model =
    -- TODO: Handle a case with four Gipf pieces in a row
    -- If not handled, it will lead to a infinite loop.
    if model.game.state /= WaitingForRemove then
        NothingToSelect

    else if selected model == [] then
        -- Nothing has been auto-selected yet. That means that we need to disambiguate between the possible removals.
        PlayerNeedsToDisambiguateRemoval

    else if snd model.autoSelected == [] then
        -- The player has no Gipf pieces to remove, so we need to wait for them to confirm.
        PlayerNeedsToConfirmRemoval

    else
        -- The  player has some Gipf pieces to remove, so we give them a chance to toggle them for removal.
        PlayerNeedsToToggleGipfPieces


type SelectionState
    = NothingToSelect
    | PlayerNeedsToConfirmRemoval
    | PlayerNeedsToToggleGipfPieces
    | PlayerNeedsToDisambiguateRemoval


{-|

    playerWithAction returns the color of the player that needs to perform the action.

-}
playerWithAction : Model -> Color
playerWithAction model =
    if model.game.state == WaitingForRemove && model.game.currentPlayerFourStones == [] then
        reverseColor model.game.currentColor

    else
        model.game.currentColor


{-|

    viewCurrentAction displays a control in the top right corner that shows the current action.

-}
viewCurrentAction : Model -> Svg Msg
viewCurrentAction model =
    if model.allowActions then
        case currentSelectionState model of
            NothingToSelect ->
                if model.game.state == WaitingForMove then
                    if
                        -- TODO: Move this logic to Gipf.elm
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

                else if model.game.state == BlackWon then
                    g []
                        [ drawPiece (Piece ( 8, 10 ) Black Regular)
                        , drawMultilineTextAtCoord "Black\nWon!" ( 8, 10 ) -25 35 10
                        ]

                else
                    -- white won
                    g []
                        [ drawPiece (Piece ( 8, 10 ) White Regular)
                        , drawMultilineTextAtCoord "White\nWon!" ( 8, 10 ) -25 35 10
                        ]

            PlayerNeedsToDisambiguateRemoval ->
                g []
                    [ drawPiece (Piece ( 8, 10 ) (playerWithAction model) Regular)
                    , drawLightMark ( 8, 10 )
                    , drawMultilineTextAtCoord "Select a\ngroup to\nremove" ( 8, 10 ) -70 -10 12
                    ]

            PlayerNeedsToToggleGipfPieces ->
                g []
                    [ drawPiece (Piece ( 8, 10 ) (playerWithAction model) Gipf)
                    , drawLightMark ( 8, 10 )
                    , drawMultilineTextAtCoord "Toggle Gipf\npieces\nto remove" ( 8, 10 ) -80 -10 12
                    ]

            PlayerNeedsToConfirmRemoval ->
                g []
                    [ drawPiece (Piece ( 8, 10 ) (playerWithAction model) Regular)
                    , drawMultilineTextAtCoord "Click to\nconfirm" ( 8, 10 ) -70 -10 12
                    ]

    else
        g [] [ drawPiece (Piece ( 8, 10 ) (playerWithAction model) Regular) ]


{-|

    viewSelectionAndRemoval displays any pieces that can be removed from the board, and allows the player to select them.
    It does a few things:
    - Designates each piece that can be used for disambiguation with a light mark.
    - Attaches enter/leave/click events to all these points.
    - Draws dark crosses on all points that will be selected for removal upon hover.
    - When one of the multiple groups is selected, process click events to select the Gipf pieces to remove.

-}
viewSelectionAndRemoval : Model -> Svg Msg
viewSelectionAndRemoval model =
    let
        markedForRemoval =
            List.map drawDarkCross (selected model ++ model.gipfsSelected)
                ++ List.map drawLightMark (snd model.autoSelected)
    in
    case currentSelectionState model of
        NothingToSelect ->
            g [] []

        PlayerNeedsToDisambiguateRemoval ->
            g []
                (List.map
                    drawLightMark
                    (disambiguateRemovalCoords model.game)
                    ++ List.map
                        (\p -> drawClickPoint p RemovalDisambiguationEnter RemovalDisambiguationLeave RemovalDisambiguationClick)
                        (disambiguateRemovalCoords model.game)
                    ++ (case model.selectedToDisambiguate of
                            Nothing ->
                                []

                            Just coord ->
                                let
                                    ( auto, gipfs ) =
                                        autoSelectToRemoveWithDisambiguation model.game coord

                                    willBeRemoved =
                                        auto ++ gipfs
                                in
                                List.map drawDarkCross willBeRemoved
                       )
                )

        PlayerNeedsToToggleGipfPieces ->
            g []
                (markedForRemoval
                    ++ List.map (\p -> drawClickPoint p GipfEnter GipfEnter GipfClicked) (snd model.autoSelected)
                )

        PlayerNeedsToConfirmRemoval ->
            g [] markedForRemoval


viewConfirmRemoveButton : Model -> Html Msg
viewConfirmRemoveButton model =
    if selected model == [] then
        div [] []

    else
        div []
            [ button
                [ style "position" "absolute"
                , style "top" "75px"
                , style "left" "543px"
                , onClick RemovePieces
                ]
                [ text "Remove" ]
            , if (List.length model.game.currentPlayerFourStones > 1) || (List.length model.game.otherPlayerFourStones > 1) then
                button
                    [ style "position" "absolute"
                    , style "top" "100px"
                    , style "left" "543px"
                    , onClick CancelRemovePieces
                    ]
                    [ text "Cancel" ]

              else
                div [] []
            ]


view : Model -> Html Msg
view model =
    div [ style "position" "relative" ]
        [ svg
            [ width "610"
            , height "730"
            , viewBox "0 0 610 730"
            , Svg.Attributes.style "user-select: none; -webkit-user-select: none; -moz-user-select: none; -ms-user-select: none;"
            ]
            [ viewEmptyBoard
            , viewPiecesCounts model
            , viewCurrentAction model
            , viewPieces model
            , if model.allowActions then
                viewPossibleMoves model

              else
                g [] []
            , if model.allowActions then
                viewSelectionAndRemoval model

              else
                g [] []
            ]
        , if model.allowActions then
            viewConfirmRemoveButton model

          else
            div [] []
        , if model.showDebug then
            div
                [ style "text-align" "left"
                , style "width" "610px"
                , style "word-wrap" "break-word"
                ]
                [ p [] [ text (actionsToString model.game.actionHistory) ]
                , p [ fontSize "6" ] [ text (Debug.toString model.game) ]
                ]

          else
            div [] []
        ]


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }
