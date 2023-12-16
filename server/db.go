// db.go contains all the database related functions.

package main

import (
	"database/sql"
	"fmt"
	"log"
	"sort"
)

// Database initialization

var db *sql.DB

func initDB() {
	var err error
	db, err = sql.Open("sqlite3", "file:./games.db?cache=shared&mode=rwc&_journal_mode=WAL&_synchronous=NORMAL&_busy_timeout=5000")
	if err != nil {
		log.Fatal(err)
	}

	sqlStmt := `
	CREATE TABLE IF NOT EXISTS users (
		id INTEGER PRIMARY KEY AUTOINCREMENT,
		email TEXT UNIQUE,
		email_verified INTEGER DEFAULT 0,
		password_hash TEXT,
		screen_name TEXT UNIQUE,
		is_admin INTEGER DEFAULT 0,
		creation_time REAL DEFAULT ((julianday('now') - 2440587.5)*86400000)
	);

	CREATE TABLE IF NOT EXISTS tokens (
		user_id INTEGER,
		token TEXT,
		creation_time REAL DEFAULT ((julianday('now') - 2440587.5)*86400000),
		PRIMARY KEY (user_id, token), 
		FOREIGN KEY (user_id) REFERENCES users(user_id)
	);

    CREATE TABLE IF NOT EXISTS games (
		id INTEGER PRIMARY KEY AUTOINCREMENT, 
		type TEXT, -- type of the game (such as Gipf, ...)

		-- white_user_id is the foreign key  of the user who playes as white
		-- black_user_id is the id of the user who playes as black
		-- white_user_id and black_user_id can be null if the game is played by a guest
		white_user_id INTEGER DEFAULT -1,
		black_user_id INTEGER DEFAULT -1,

		white_token TEXT,
		black_token TEXT,
		viewer_token TEXT,
		game_over INTEGER DEFAULT 0,
		game_result TEXT DEFAULT "",
		creation_time REAL DEFAULT ((julianday('now') - 2440587.5)*86400000)
	);

	CREATE TABLE IF NOT EXISTS actions (
		game_id INTEGER, 
		-- the number of the action in the sequence (starting from 1)
		action_num INTEGER,
		action TEXT,
		-- an MD5 hash of the (game_id, action_num, player_key, action), calculated by the client, for client integrity verification
		action_signature TEXT, 
		creation_time REAL DEFAULT ((julianday('now') - 2440587.5)*86400000), 
		PRIMARY KEY (game_id, action_num)
	);
    `
	_, err = db.Exec(sqlStmt)
	if err != nil {
		log.Fatalf("%q: %s\n", err, sqlStmt)
		return
	}
}

type PlayerType int

const (
	WhitePlayer PlayerType = iota
	BlackPlayer
	Viewer
	InvalidPlayer
)

func (p PlayerType) String() string {
	switch p {
	case WhitePlayer:
		return "white"
	case BlackPlayer:
		return "black"
	case Viewer:
		return "viewer"
	default:
		return "invalid"
	}
}

// validateGameToken checks if the given token is valid player token for the given game, and returns the player type and the game token.
// Note: the game token is not necessarily the same as the given token (which could just help identify the user).
//
//	The token is valid if:
//	a) the token is either the white token or the black token, or
//	b) the token belongs to the user who is playing as white or black in the game, or
//	c) the token is the viewer token, or
//	d) the viewer token associated with the game is "", which means that the game is public and anyone can view it.
func validateGameToken(gameID int, token Token) (PlayerType, Token) {
	var whiteToken, blackToken, viewerToken Token
	var whiteUserID, blackUserID int
	err := db.QueryRow(
		"SELECT white_token, black_token, viewer_token, white_user_id, black_user_id FROM games WHERE id = ?",
		gameID).Scan(&whiteToken, &blackToken, &viewerToken, &whiteUserID, &blackUserID)
	if err != nil {
		return InvalidPlayer, "" // the game does not exist
	}
	if token != "" && token == whiteToken {
		return WhitePlayer, whiteToken
	} else if token != "" && token == blackToken {
		return BlackPlayer, blackToken
	}

	var userID int
	err = db.QueryRow("SELECT user_id FROM tokens WHERE token = ?", token).Scan(&userID)
	if err == nil {
		if userID == whiteUserID {
			return WhitePlayer, whiteToken
		} else if userID == blackUserID {
			return BlackPlayer, blackToken
		}
	}
	if token == viewerToken && viewerToken != "" {
		return Viewer, viewerToken
	}
	return InvalidPlayer, ""
}

type NewGameRequest struct {
	Type          string `json:"type"`
	WhiteUsername string `json:"white_username"`
	BlackUsername string `json:"black_username"`
	Public        bool   `json:"public"`
}

func getUserIDFromUsername(username string) (int, error) {
	var userID int
	err := db.QueryRow("SELECT id FROM users WHERE username = ?", username).Scan(&userID)
	if err != nil {
		return -1, err
	}
	return userID, nil
}

type NewGame struct {
	ID          int   `json:"id"`
	WhiteToken  Token `json:"white_token"`
	BlackToken  Token `json:"black_token"`
	ViewerToken Token `json:"viewer_token"`
}

func createGame(request NewGameRequest) (*NewGame, error) {
	var whiteToken, blackToken, viewerToken Token

	if request.WhiteUsername != "" {
		_, err := getUserIDFromUsername(request.WhiteUsername)
		if err != nil {
			return nil, err
		}
	}

	if request.BlackUsername != "" {
		_, err := getUserIDFromUsername(request.BlackUsername)
		if err != nil {
			return nil, err
		}
	}

	whiteToken = generateToken()
	blackToken = generateToken()
	if request.Public {
		viewerToken = generateToken()
	}

	var whiteUserID, blackUserID int
	var err error
	if request.WhiteUsername == "" {
		whiteUserID = -1
	} else {
		whiteUserID, err = getUserIDFromUsername(request.WhiteUsername)
		if err != nil {
			return nil, err
		}
	}
	if request.BlackUsername == "" {
		blackUserID = -1
	} else {
		blackUserID, err = getUserIDFromUsername(request.BlackUsername)
		if err != nil {
			return nil, err
		}
	}

	res, err := db.Exec(
		"INSERT INTO games(type, white_user_id, black_user_id, white_token, black_token, viewer_token) VALUES(?, ?, ?, ?, ?, ?)",
		request.Type, whiteUserID, blackUserID, whiteToken, blackToken, viewerToken)
	if err != nil {
		return nil, err
	}

	gameID, err := res.LastInsertId()
	if err != nil {
		return nil, err
	}

	return &NewGame{
		ID:          int(gameID),
		WhiteToken:  whiteToken,
		BlackToken:  blackToken,
		ViewerToken: viewerToken,
	}, nil
}

func markGameAsFinished(gameID int, result string) error {
	_, err := db.Exec("UPDATE games SET game_over = 1, game_result = ? WHERE id = ?", result, gameID)
	return err
}

// checkGameStatus checks the game's status and returns an error if the game is finished or other issues are found.
func checkGameStatus(gameID int) error {
	var gameOver int
	err := db.QueryRow("SELECT game_over FROM games WHERE id = ?", gameID).Scan(&gameOver)
	if err != nil {
		return err
	}
	if gameOver == 1 {
		return fmt.Errorf("game is over")
	}
	return nil
}

// Server administration

// User is a struct that represents a user in the database.

func listUsers() ([]*User, error) {
	query := `
    SELECT u.id, u.email, u.screen_name, u.creation_time, t.token
    FROM users u
    LEFT JOIN (
        SELECT token, user_id
        FROM tokens
        ORDER BY id DESC
        LIMIT 1
    ) t ON u.id = t.user_id
	ORDER BY u.created_time DESC
`
	rows, err := db.Query(query)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	var users []*User
	for rows.Next() {
		var token sql.NullString
		var user User
		var creationTime float64

		if err := rows.Scan(&user.Id, &user.Email, &user.ScreenName, &creationTime, &token); err != nil {
			return nil, err
		}
		user.CreationTime = int(creationTime)
		users = append(users, &user)

	}
	return users, nil
}

type Game struct {
	ID           int    `json:"id"`
	Type         string `json:"type"`
	WhiteUser    string `json:"white_user"`
	BlackUser    string `json:"black_user"`
	WhiteToken   string `json:"white_token"`
	BlackToken   string `json:"black_token"`
	ViewerToken  string `json:"viewer_token"`
	GameOver     bool   `json:"game_over"`
	GameResult   string `json:"game_result"`
	CreationTime int    `json:"creation_time"`
	NumActions   int    `json:"num_actions"`
	GameRecord   string `json:"game_record"`
}

func getGame(id int) (*Game, error) {
	query := `
		SELECT 
			g.id, g.type, u1.username, u2.username, g.white_token, g.black_token, g.viewer_token, g.game_over, g.game_result, g.creation_time,
			COUNT(a.action_num) AS num_actions, 
			COALESCE(GROUP_CONCAT(a.action ORDER BY a.creation_time, ', '), '')  AS game_record
		FROM games g
		LEFT JOIN users u1 ON g.white_user_id = u1.id
		LEFT JOIN users u2 ON g.black_user_id = u2.id
		LEFT JOIN actions a ON g.id = a.game_id
		WHERE g.id = ?
		GROUP BY g.id
	`
	var game Game
	var whiteUser, blackUser sql.NullString
	var creationTime float64

	err := db.QueryRow(query, id).Scan(&game.ID, &game.Type, &whiteUser, &blackUser, &game.WhiteToken, &game.BlackToken, &game.ViewerToken,
		&game.GameOver, &game.GameResult, &creationTime, &game.NumActions, &game.GameRecord)
	if err != nil {
		return nil, err
	}
	game.CreationTime = int(creationTime)

	if whiteUser.Valid {
		game.WhiteUser = whiteUser.String
	}
	if blackUser.Valid {
		game.BlackUser = blackUser.String
	}

	return &game, nil
}

func listGames() ([]Game, error) {
	query := `
		SELECT 
			g.id, g.type, u1.username, u2.username, g.white_token, g.black_token, g.viewer_token, g.game_over, g.game_result, g.creation_time,
			COUNT(a.action_num) AS num_actions, 
            COALESCE(GROUP_CONCAT(a.action ORDER BY a.creation_time, ', '), '')  AS game_record
		FROM games g
		LEFT JOIN users u1 ON g.white_user_id = u1.id
		LEFT JOIN users u2 ON g.black_user_id = u2.id
		LEFT JOIN actions a ON g.id = a.game_id
		GROUP BY g.id
	`
	rows, err := db.Query(query)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	games := make([]Game, 0)
	for rows.Next() {
		var game Game
		var whiteUser, blackUser sql.NullString
		var creationTime float64

		if err := rows.Scan(&game.ID, &game.Type, &whiteUser, &blackUser, &game.WhiteToken, &game.BlackToken, &game.ViewerToken,
			&game.GameOver, &game.GameResult, &creationTime, &game.NumActions, &game.GameRecord); err != nil {
			return nil, err
		}
		game.CreationTime = int(creationTime)

		if whiteUser.Valid {
			game.WhiteUser = whiteUser.String
		}
		if blackUser.Valid {
			game.BlackUser = blackUser.String
		}

		games = append(games, game)
	}

	sort.Slice(games, func(i, j int) bool {
		return games[i].CreationTime > games[j].CreationTime
	})

	return games, nil
}
