# The Gipf Project Server

## Overview

This is a server implementation for Gipf Project games.

It's written in Go (for the server), and stores everything in a Postgres database (or SQLite for local development).

## Server Architecture

### MVP

The MVP artecture is pretty simple:

    - /newgame will create a new game, and produce three tokens: one for black's moves, one for white, and one to view the game
    - /game?id=ID&token=TOKEN sends full game recording up to this point, assuming the token is valid. Each action is just a string.
    - /action expects a POST with json {game_id, token, action, action_number, next_action_owner}. Assuming it's correct (e.g., token is either black's or white's,
    the current action matches the token owner, the action number is one greater than the 
    current number of actions), a new action is added to the database, and the next action
    owner is also updated.


The database architecture is therefore very simple:
    - A table of games, (game ID, game type, creation time, current action owner, tokens)
    - A table of actions (game ID, action ID, action string, creation time)