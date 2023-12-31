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
   action signing, and information about the game type, current `actions` and
   `starting_position`.

2. A triple `(game_type, starting_positions, actions)` should be enough for `GipfBoard.elm` 
   or any other board to completely recreate the game state.

   Note that for GIPF in particular, `starting_position` is excessive because `game_type`
   has enough information to create it - there are only three starting position: empty
   board (for Tournament GIPF), and 6 pieces in standard locations which are either regular
   or GIPF pieces for Basic and Standard GIPF games, respectively.

   However, for other games (e.g., LYNGK which requires a randomized starting position) knowing
   the `game_type` won't be enough, unless we want both clients to work collaborately on the
   starting position.

3. An exposed `GipfBoard.lastAction` method that should be called and the respective action
   should be sent to the server if the message belongs to the list `updateAction` messages.