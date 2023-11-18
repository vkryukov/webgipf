module DebugTools exposing (..)

import Gipf exposing (..)


justGame : Maybe Game -> Game
justGame game =
    case game of
        Just g ->
            g

        Nothing ->
            emptyGame


justPieces : Maybe (List Piece) -> List Piece
justPieces pieces =
    case pieces of
        Just ps ->
            ps

        Nothing ->
            []
