## Interface between PlayGame and individual games

PlayGame should be the target-game agnostic (it's currently not). One clear evidence of that
is that it imports `Gipf.elm` module for its `Gipf.lastAction` method; but it should only import
`GipfBoard.elm` which should handle all the low-level details.

Another example of a "too intimate" connection is the knowledge of which actions (such
as `MoveMade` and `RemovePieces`) by `GipfBoard.elm` should result in an action been sent
up to the server.

Here is a redesigned minimal interface between PlayGame and different games:

1. PlayGame receives `(game_id, player_token)` from `App.elm`, and after joining a game,
   translates that into `JoinGameResponse`, which contains the information about the players,
   the `game_token`, which replaces the `user_token` and should be used for all future 
   action signing, and information about the game type and current `actions`.

2. A touple `(game_type, actions)` should be enough for `GipfBoard.elm` 
   or any other board to completely recreate the game state.

3. An exposed `GipfBoard.lastAction` method that should be called and the respective action
   should be sent to the server if the message belongs to the list `updateAction` messages.


## Toolbar for GipfBoard.elm

Right now, `GipfBoard.view` displays buttons and messages in a few places:

1. `viewConfirmRemoveButton`
   We show Remove and Cancel button, based on certain conditions using
   viewSmall(Primary|Secondary)Button

2. `viewCurrentAction`
   Uses drawMultilineTextAtCoord to display various statuses

3. `viewSelectionAndRemoval`
   Mostly affects the boards depending on the various state of
   the position (`currentSelectionState model`)

4. `viewPossibleMoves`
   Only affects edge points, so can be ignored for now

We can replace the 1 and 2 with the toolbar and a status text below it.
The toolbar will show a bunch of buttons, whether primary or secondary,
based on a certain condition.