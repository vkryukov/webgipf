## Overall architecture and UI

[X] Main.elm should incorporate Auth.elm 

[ ] Main.elm should incorporate Admin.elm

[X] Incorporate tailwindcss; ./run_dev.sh script should add tailwindcss rebuilding

[X] Make sure the logging is consistent everywhere; maybe log client access etc. to 
    the database?

[X] Dev mode should have an option to use a MockEmailServer for sending email.

[ ] Compile and run the latest version on playgipf.com

## Improving Sign-in and sign-up

[X] UI should not allow sign-in or sign-up if any of the fields are empty

[X] UI should not allow sing-up if the passwords do not match

[X] The server should return an error message explaining why sign-up didn't work 
    (e.g., account exists already, etc.)

    The challenge: we use the same command (LoginReceived) for both SignIn and SignUp, therefore
    when we receive an error, we can't tell what exactly caused it.

    Solution: we should always return a JSON with an error message, instead of 404. That will require
    changing updateModelWithUserStatus as well, as it expects either Http.Error or User field. Instead of User
    , it should be either User or Error message.

[X] Change updateModelWithUserStatus to reflect the server sending an error message as per above

[X] Server should check whether we can send an email, and return an error if not
    - If that's the case, the entry in the database should NOT be created

[X] The server should still check that the passwords match during registration

[X] Use Token type everywhere where it matters in auth.go
 
[ ] Decide what to do with the screen name that currently have to be unique. For the MVP,
    requiring it to be unique is probably the best tactic.

[ ] During the registration, if an error occurs, a corresponding field should be highlighted.

## User actions for signed-in user

A sign-in user should be able to:

[X] Create new games (selecting type of the game, selecting black or white)

[X] See a list of games that can be joined, and join these games

[X] See a list of games they started

[ ] Know which of their own games require their move

[X] Select a game, have the board displayed, and be able to make moves

[ ] (When not in a game) Receive notification about new moves, and see which games have move
    updates since last visit

[ ] View and replay their past games

[ ] Resend their verification link, and click on it to verify email

[ ] See the status of their email verification in the status bar


## Improve play game experience

[X] Don't show all the auxialry information

[X] Refreshing the screen should bring back the game

[ ] Stop/restart websocket connection on joining/leaving the play game screen as per Elm book

[ ] Display game type

[X] Correctly handle starting position for Basic and Standard game

[X] PlayGame should utilize a generic interface, not deeply embed knowledge of Gipf specifically


## Improve Home page signed in experience

[X] Num of actions on the first screen should be correct (currently 0 for a game just started)

[X] Cancel should indeed cancel the games

[ ] Differentiate between public and private games

[ ] Allow creation of private games, with the ability to let people join them


## Admin actions

[ ] An admin user (and only an admin user) should have access to various admin end points, that should enforce
    admin authentication.

[ ] List of all users (id, email, screen name, verification status)

[ ] List of all games (id, white/black player names, number of moves, current action)

[ ] Ability to view individual game