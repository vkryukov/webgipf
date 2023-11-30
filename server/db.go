// db.go contains all the database related functions.

package main

import (
	"crypto/rand"
	"database/sql"
	"fmt"
	"log"
	"sort"
	"strings"

	"golang.org/x/crypto/bcrypt"
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
		username TEXT UNIQUE,
		password TEXT,
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
		action_id INTEGER,
		-- the number of the action in the sequence (starting from 1)
		action_num INTEGER,
		action TEXT,
		-- an MD5 hash of the (game_id, action_num, player_key, action), calculated by the client, for client integrity verification
		action_signature TEXT, 
		creation_time REAL DEFAULT ((julianday('now') - 2440587.5)*86400000), 
		PRIMARY KEY (game_id, action_id)
	);
    `
	_, err = db.Exec(sqlStmt)
	if err != nil {
		log.Fatalf("%q: %s\n", err, sqlStmt)
		return
	}
}

// Authentication

func comparePasswords(hashedPwd string, plainPwd string) bool {
	return bcrypt.CompareHashAndPassword([]byte(hashedPwd), []byte(plainPwd)) == nil
}

func authenticateUser(username string, password string) (int, error) {
	var userID int
	var hashedPwd string

	err := db.QueryRow("SELECT id, password FROM users WHERE username = ?", username).Scan(&userID, &hashedPwd)
	if err != nil {
		return -1, err
	}

	if !comparePasswords(hashedPwd, password) {
		return -1, fmt.Errorf("wrong password")
	}

	return userID, nil
}

func registerUser(username string, password string) (int, error) {
	hashedPwd, err := bcrypt.GenerateFromPassword([]byte(password), bcrypt.DefaultCost)
	if err != nil {
		return -1, err
	}

	res, err := db.Exec("INSERT INTO users(username, password) VALUES(?, ?)", username, hashedPwd)
	if err != nil {
		log.Printf("Error inserting user %s into database: %v", username, err)
		return -1, err
	}

	userID, err := res.LastInsertId()
	if err != nil {
		return -1, err
	}

	return int(userID), nil
}

type Token string

func generateToken() Token {
	b := make([]byte, 16)
	rand.Read(b)
	return Token(fmt.Sprintf("%x", b))
}

func addNewTokenToUser(userID int) (Token, error) {
	token := generateToken()

	_, err := db.Exec("INSERT INTO tokens(user_id, token) VALUES(?, ?)", userID, token)
	if err != nil {
		return "", err
	}

	return token, nil
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
		"SELECT white_token, black_token, viewerToken, white_user_id, black_user_id FROM games WHERE id = ?",
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

	res, err := db.Exec(
		"INSERT INTO games(type, white_username, black_username, white_token, black_token, viewer_token) VALUES(?, ?, ?, ?, ?, ?)",
		request.Type, request.WhiteUsername, request.BlackUsername, whiteToken, blackToken, viewerToken)
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

func getNumberOfMoves(gameID int) (int, error) {
	var numMoves int
	err := db.QueryRow("SELECT COUNT(*) FROM actions WHERE game_id = ?", gameID).Scan(&numMoves)
	if err != nil {
		return -1, err
	}
	return numMoves, nil
}

func getAllMoves(gameID int) (string, error) {
	var moves []string
	rows, err := db.Query("SELECT action FROM actions WHERE game_id = ? ORDER BY creation_time", gameID)
	if err != nil {
		return "", err
	}
	defer rows.Close()

	for rows.Next() {
		var move string
		if err := rows.Scan(&move); err != nil {
			return "", err
		}
		moves = append(moves, move)
	}

	return strings.Join(moves, " "), nil
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

// checkMoveValidity checks if the move number is correct and returns an error if it's not.
func checkMoveValidity(gameID int, moveNum int) error {
	numMoves, err := getNumberOfMoves(gameID)
	if err != nil {
		return err
	}
	if moveNum != numMoves+1 {
		return fmt.Errorf("invalid move number: got %d, expected %d", moveNum, numMoves+1)
	}
	return nil
}

// Server administration

// User is a struct that represents a user in the database.
type User struct {
	ID           int      `json:"id"`
	Username     string   `json:"username"`
	CreationTime int      `json:"creation_time"`
	Tokens       []string `json:"tokens"`
}

func listUsers() ([]User, error) {
	// Query to join users and tokens tables
	query := `
        SELECT u.id, u.username, u.creation_time, t.token
        FROM users u
        LEFT JOIN tokens t ON u.id = t.user_id
    `
	rows, err := db.Query(query)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	users := make(map[int]*User)
	for rows.Next() {
		var token sql.NullString
		var user User
		var creationTime float64

		if err := rows.Scan(&user.ID, &user.Username, &creationTime, &token); err != nil {
			return nil, err
		}
		user.CreationTime = int(creationTime)

		// Check if the user already exists in the map
		if existingUser, exists := users[user.ID]; exists {
			// Append the token to the existing user's tokens if not null
			if token.Valid {
				existingUser.Tokens = append(existingUser.Tokens, token.String)
			}
		} else {
			// If the token is valid, initialize the Tokens slice
			if token.Valid {
				user.Tokens = []string{token.String}
			}
			users[user.ID] = &user
		}
	}

	// Convert the map to a slice of users
	usersSlice := make([]User, 0, len(users))
	for _, user := range users {
		usersSlice = append(usersSlice, *user)
	}

	sort.Slice(usersSlice, func(i, j int) bool {
		return usersSlice[i].CreationTime > usersSlice[j].CreationTime
	})

	return usersSlice, nil
}
