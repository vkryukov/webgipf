package main

import (
	"crypto/rand"
	"database/sql"
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"strconv"
	"sync"

	"github.com/gorilla/websocket"
	_ "github.com/mattn/go-sqlite3"
	"golang.org/x/crypto/bcrypt"
)

// Database initialization

var db *sql.DB

func initDB() {
	var err error
	db, err = sql.Open("sqlite3", "./games.db")
	if err != nil {
		log.Fatal(err)
	}

	sqlStmt := `
	CREATE TABLE IF NOT EXISTS users (
		id INTEGER PRIMARY KEY AUTOINCREMENT,
		username TEXT UNIQUE,
		password TEXT,
		creation_time DATETIME DEFAULT CURRENT_TIMESTAMP
	);

	CREATE TABLE IF NOT EXISTS tokens (
		user_id INTEGER,
		token TEXT,
		creation_time DATETIME DEFAULT CURRENT_TIMESTAMP,
		PRIMARY KEY (user_id, token)
	);

    CREATE TABLE IF NOT EXISTS games (
		id INTEGER PRIMARY KEY AUTOINCREMENT, 
		type TEXT, -- type of the game (such as Gipf, ...)

		-- white_user_id is the foreign key  of the user who playes as white
		-- black_user_id is the id of the user who playes as black
		-- white_user_id and black_user_id can be null if the game is played by a guest
		white_user_id INTEGER,
		black_user_id INTEGER,

		white_token TEXT,
		black_token TEXT,
		viewer_token TEXT,
		current_action INTEGER,
		creation_time DATETIME DEFAULT CURRENT_TIMESTAMP
	);

	CREATE TABLE IF NOT EXISTS actions (
		game_id INTEGER, 
		action_id INTEGER, 
		action TEXT, 
		creation_time DATETIME DEFAULT CURRENT_TIMESTAMP, 
		PRIMARY KEY (game_id, action_id)
	);
    `
	_, err = db.Exec(sqlStmt)
	if err != nil {
		log.Fatalf("%q: %s\n", err, sqlStmt)
		return
	}
}

// Utility functions

func generateToken() string {
	b := make([]byte, 16)
	rand.Read(b)
	return fmt.Sprintf("%x", b)
}

// Authentication

// User is a struct that represents a user in the database.
type User struct {
	ID       int    `json:"id"`
	Username string `json:"username"`
	Password string `json:"password"`
}

// generateAndStoreToken generates a new token, stores it in the database for the given user ID, and returns it.
func generateAndReturnToken(userID int, w http.ResponseWriter) {
	token := generateToken()

	_, err := db.Exec("INSERT INTO tokens(user_id, token) VALUES(?, ?)", userID, token)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	response := map[string]interface{}{
		"token": token,
	}
	jsonResponse, err := json.Marshal(response)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	w.Write(jsonResponse)

}

func hashAndSalt(pwd []byte) (string, error) {
	hash, err := bcrypt.GenerateFromPassword(pwd, bcrypt.MinCost)
	if err != nil {
		return "", err
	}
	return string(hash), nil
}

func comparePasswords(hashedPwd string, plainPwd []byte) bool {
	byteHash := []byte(hashedPwd)
	err := bcrypt.CompareHashAndPassword(byteHash, plainPwd)
	if err != nil {
		return false
	}
	return true
}

// authenticateUser is an HTTP handler that authenticates a user with a given username and password.
// It returns a token that can be used to authenticate future requests.
func authenticateUser(w http.ResponseWriter, r *http.Request) {
	var user User
	err := json.NewDecoder(r.Body).Decode(&user)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	// Check if the username and password are valid
	var id int
	var hashedPassword string
	err = db.QueryRow("SELECT id, password FROM users WHERE username = ?", user.Username).Scan(&id, &hashedPassword)
	if err != nil || !comparePasswords(hashedPassword, []byte(user.Password)) {
		http.Error(w, "Invalid username or password", http.StatusUnauthorized)
		return
	}

	generateAndReturnToken(id, w)
}

// registerUser is an HTTP handler that registers a user with a given username and password.
// It returns a token that can be used to authenticate future requests.
func registerUser(w http.ResponseWriter, r *http.Request) {
	var user User
	err := json.NewDecoder(r.Body).Decode(&user)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	hashedPassword, err := hashAndSalt([]byte(user.Password))
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	result, err := db.Exec("INSERT INTO users(username, password) VALUES(?, ?)", user.Username, hashedPassword)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	// Get the ID of the inserted user
	userID, err := result.LastInsertId()
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	generateAndReturnToken(int(userID), w)
}

// WebSockets

var (
	connectedUsers   = make(map[int][]*websocket.Conn)
	connectedUsersMu sync.Mutex
)

var allowedOrigins = map[string]bool{
	"http://localhost":                true,
	"https://your-allowed-origin.com": true,
}

var upgrader = websocket.Upgrader{
	CheckOrigin: func(r *http.Request) bool {
		// Allow connections with a null origin (for local file testing)
		origin := r.Header.Get("Origin")
		return origin == "" || origin == "null" || allowedOrigins[origin]
	},
}

type ActionOwner int

const (
	WhiteAction ActionOwner = iota
	BlackAction
)

func handleNewGame(w http.ResponseWriter, r *http.Request) {
	// Parse the URL query parameters
	params := r.URL.Query()

	// Extract the game type from the query parameters
	gameType := params.Get("type")
	if gameType == "" {
		http.Error(w, "Missing game type", http.StatusBadRequest)
		return
	}

	// Generate three pseudo-random tokens
	whiteToken := generateToken()
	blackToken := generateToken()
	viewerToken := generateToken()

	// Create a new game record in the database
	stmt, err := db.Prepare("INSERT INTO games(type, white_token, black_token, viewer_token, current_action) VALUES(?, ?, ?, ?, ?)")
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	res, err := stmt.Exec(gameType, whiteToken, blackToken, viewerToken, WhiteAction)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	// Get the ID of the new game record
	id, err := res.LastInsertId()
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	log.Printf("Created a new game with ID %d and type %s\n", id, gameType)
	log.Printf("Generated tokens - White: %s, Black: %s, Viewer: %s\n", whiteToken, blackToken, viewerToken)

	// Create the response
	response := map[string]interface{}{
		"id":     id,
		"white":  whiteToken,
		"black":  blackToken,
		"viewer": viewerToken,
	}

	// Convert the response to JSON
	jsonResponse, err := json.Marshal(response)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	// Set the content type to JSON and send the response
	w.Header().Set("Content-Type", "application/json")
	w.Write(jsonResponse)
}

func handleGame(w http.ResponseWriter, r *http.Request) {
	// Parse the URL query parameters
	params := r.URL.Query()

	// Extract the game id and token from the query parameters
	gameIDStr := params.Get("id")
	token := params.Get("token")
	if gameIDStr == "" || token == "" {
		http.Error(w, "Missing game id or token", http.StatusBadRequest)
		return
	}

	// Convert the game id to an integer
	gameID, err := strconv.Atoi(gameIDStr)
	if err != nil {
		http.Error(w, "Invalid game id", http.StatusBadRequest)
		return
	}

	// Check if the game id and token are valid
	var whiteToken, blackToken, viewerToken string
	err = db.QueryRow("SELECT white_token, black_token, viewer_token FROM games WHERE id = ?", gameID).Scan(&whiteToken, &blackToken, &viewerToken)
	if err != nil {
		http.Error(w, "Invalid game id", http.StatusBadRequest)
		return
	}
	if token != whiteToken && token != blackToken && token != viewerToken {
		http.Error(w, "Invalid token", http.StatusBadRequest)
		return
	}

	// Get the list of actions for the game
	rows, err := db.Query("SELECT action FROM actions WHERE game_id = ? ORDER BY creation_time", gameID)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	defer rows.Close()

	// Convert the actions to a slice of Action structs
	actions := make([]string, 0)
	for rows.Next() {
		var action string
		if err := rows.Scan(&action); err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
		actions = append(actions, action)
	}

	// Convert the actions to JSON
	jsonResponse, err := json.Marshal(actions)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	// Upgrade the connection to a WebSocket
	conn, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		log.Printf("Failed to upgrade the connection: %v", err)
		return
	}
	// defer conn.Close()

	// Lock the mutex before accessing the shared data
	connectedUsersMu.Lock()

	// Add the client to the list of connected clients for the game
	connectedUsers[gameID] = append(connectedUsers[gameID], conn)

	// Unlock the mutex after you're done with the shared data
	connectedUsersMu.Unlock()

	// Send the JSON response over the WebSocket
	err = conn.WriteMessage(websocket.TextMessage, jsonResponse)
	if err != nil {
		log.Printf("Failed to send JSON response: %v", err)
	}
}

type Action struct {
	GameID          int    `json:"game_id"`
	Token           string `json:"token"`
	Action          string `json:"action"`
	ActionNumber    int    `json:"action_number"`
	NextActionOwner int    `json:"next_action_owner"`
}

func handleAction(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		http.Error(w, "Invalid request method", http.StatusMethodNotAllowed)
		return
	}

	// Decode the request body into an Action struct
	var action Action
	err := json.NewDecoder(r.Body).Decode(&action)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	// Check if the game id and token are valid
	var whiteToken, blackToken, viewerToken string
	var currentAction int
	err = db.QueryRow("SELECT white_token, black_token, viewer_token, current_action FROM games WHERE id = ?", action.GameID).Scan(&whiteToken, &blackToken, &viewerToken, &currentAction)
	if err != nil {
		http.Error(w, "Invalid game id", http.StatusBadRequest)
		return
	}
	if action.Token == viewerToken || (currentAction == int(WhiteAction) && action.Token != whiteToken) || (currentAction == int(BlackAction) && action.Token != blackToken) {
		http.Error(w, "Invalid token", http.StatusBadRequest)
		return
	}

	// Check if the action number is valid
	var actionCount int
	err = db.QueryRow("SELECT COUNT(*) FROM actions WHERE game_id = ?", action.GameID).Scan(&actionCount)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	if action.ActionNumber != actionCount+1 {
		http.Error(w, "Invalid action number", http.StatusBadRequest)
		return
	}

	// Add the new action to the database
	_, err = db.Exec("INSERT INTO actions(game_id, action_id, action) VALUES(?, ?, ?)", action.GameID, action.ActionNumber, action.Action)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	// Update the game's current action
	_, err = db.Exec("UPDATE games SET current_action = ? WHERE id = ?", action.NextActionOwner, action.GameID)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	// Broadcast the action to all connected clients for the game
	broadcast(action.GameID, action)

	// Send a 200 OK response
	w.WriteHeader(http.StatusOK)
}

func broadcast(gameID int, action Action) {
	connectedUsersMu.Lock()
	defer connectedUsersMu.Unlock()

	var activeConnections []*websocket.Conn

	for _, conn := range connectedUsers[gameID] {
		err := conn.WriteJSON(action)
		if err != nil {
			log.Printf("Failed to send action: %v", err)
			conn.Close() // Close the failed connection
		} else {
			activeConnections = append(activeConnections, conn)
		}
	}

	connectedUsers[gameID] = activeConnections

	if len(connectedUsers[gameID]) == 0 {
		delete(connectedUsers, gameID)
	}
}

func main() {
	initDB()
	defer db.Close()

	http.HandleFunc("/authenticate", authenticateUser)
	http.HandleFunc("/register", registerUser)

	http.HandleFunc("/newgame", handleNewGame)
	http.HandleFunc("/game/", handleGame)
	http.HandleFunc("/action", handleAction)

	log.Println("Starting the server on port 8080...")
	log.Fatal(http.ListenAndServe(":8080", nil))
}
