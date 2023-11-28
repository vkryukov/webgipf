# The Gipf Project Server

## Overview

This is a server implementation for Gipf Project games.

It's written in Go, and uses SQLite (for now) as a database backend.

## Design principles

The server supports turn-based games for two players, one playing white and one playing black,
although nothing prevents us from potentially supporting more players. (But that's beyond the MVP).

Our design focuses on two objectives: Firstly, the server is designed to be indifferent to the
specifics of the game it hosts. Secondly, it incorporates features that enable clients to verify
others' moves, preventing cheating such as making unauthorized moves.

Key design elements include:

1. Game Type Support: The server is compatible with various game types, remaining neutral to
   specific game rules. It generally accepts client submissions without evaluating move legality.

2. Move Storage and Flexibility: Games are stored as a sequence of text-based moves, leaving the
   representation up to the client. This flexible approach accommodates nearly any game that can be
   delineated by a series of moves, given a defined starting position.

3. Move Number Validation: The server’s primary validation is move sequencing. It only accepts move
   N+1 following N moves.

4. Access Control through Tokens: Games are controlled by three tokens - one for each player and one
   for viewers. Moves require a player token, and game viewing necessitates a viewer token, unless
   the game is public.

5. Cheating Prevention Challenge: Despite token-based controls, cheating (like making consecutive
   moves) is still possible since the server doesn't recognize legal moves or turn order. This is
   particularly complex for games like Gipf, where players' actions aren't strictly alternating, as
   a player might need to remove some pieces following their move, which counts as another move.

6. Multiple Clients and Connectivity Issues: Challenges arise from players accessing the game on
   multiple devices and potential connectivity issues leading to missed moves.

7. Signature-Based Move Validation: To ensure move authenticity, each move is accompanied by a
   signature (e.g., a hash of move number, move, and token), allowing clients to verify the
   legitimacy of each move.

8. Handling Signature Discrepancies: If a client detects an incorrect signature, indicating someone
   has made a move out of turn, the game is halted. This suggests a critical error, likely
   irrecoverable.
