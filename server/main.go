package main

import (
	"crypto/rand"
	"database/sql"
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"sync"

	"github.com/gorilla/websocket"
	_ "github.com/mattn/go-sqlite3"
)

var (
	db               *sql.DB
	upgrader         = websocket.Upgrader{} // Use default options
	connectedUsers   = make(map[int][]*websocket.Conn)
	connectedUsersMu sync.Mutex
)

type ActionOwner int

const (
	WhiteAction ActionOwner = iota
	BlackAction
)

func initDB() {
	var err error
	db, err = sql.Open("sqlite3", "./games.db")
	if err != nil {
		log.Fatal(err)
	}

	sqlStmt := `
    CREATE TABLE IF NOT EXISTS games (
		id INTEGER PRIMARY KEY AUTOINCREMENT, 
		type TEXT,
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
		log.Printf("%q: %s\n", err, sqlStmt)
		return
	}
}

func main() {
	initDB()
	defer db.Close()

	http.HandleFunc("/newgame", handleNewGame)
	http.HandleFunc("/game/", handleGame)
	http.HandleFunc("/action", handleAction)
	log.Println("Starting the server on port 8080...")
	log.Fatal(http.ListenAndServe(":8080", nil))
}

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

func generateToken() string {
	b := make([]byte, 16)
	rand.Read(b)
	return fmt.Sprintf("%x", b)
}

func handleGame(w http.ResponseWriter, r *http.Request) {
	// Parse the URL query parameters
	params := r.URL.Query()

	// Extract the game id and token from the query parameters
	gameID := params.Get("id")
	token := params.Get("token")
	if gameID == "" || token == "" {
		http.Error(w, "Missing game id or token", http.StatusBadRequest)
		return
	}

	// Check if the game id and token are valid
	var whiteToken, blackToken, viewerToken string
	err := db.QueryRow("SELECT white_token, black_token, viewer_token FROM games WHERE id = ?", gameID).Scan(&whiteToken, &blackToken, &viewerToken)
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

	// Set the content type to JSON and send the response
	w.Header().Set("Content-Type", "application/json")
	w.Write(jsonResponse)
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

	// Send a 200 OK response
	w.WriteHeader(http.StatusOK)
}
