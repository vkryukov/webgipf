package main

import (
	"encoding/json"
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
	"strings"
	"sync"
	"time"

	"github.com/gorilla/websocket"
	_ "github.com/mattn/go-sqlite3"
)

// Pretty printing

// Define ANSI color codes
const (
	LightGrey  = "\033[37m"
	BrightBlue = "\033[94m"
	Cyan       = "\033[36m"
	Blue       = "\033[34m"
	Reset      = "\033[0m"
)

// customWriter is an io.Writer that adds color to log output
type customWriter struct {
	logFile io.Writer
}

// Write adds color codes to the log output
func (cw customWriter) Write(p []byte) (n int, err error) {
	timeStamp := time.Now().Format("01/02 15:04:05")
	coloredOutput := fmt.Sprintf("%s%s%s %s", BrightBlue, timeStamp, Reset, string(p))
	return cw.logFile.Write([]byte(coloredOutput))
}

func main() {
	log.SetFlags(0)
	log.SetOutput(&customWriter{logFile: os.Stdout})

	initDB()
	defer func() {
		_, err := db.Exec("PRAGMA wal_checkpoint;")
		if err != nil {
			log.Printf("Error executing PRAGMA wal_checkpoint: %v", err)
		}
		err = db.Close()
		if err != nil {
			log.Printf("Error closing database: %v", err)
		}
	}()

	// User management
	http.HandleFunc("/authenticate", enableCors(authenticateUserHandler))
	http.HandleFunc("/register", enableCors(registerUserHandler))
	http.HandleFunc("/changepassword", enableCors(changePasswordHandler))

	// Game management
	http.HandleFunc("/newgame", enableCors(createNewGameHandler))

	// WebSockets
	http.HandleFunc("/ws", handleWebSocket)

	// Server administration
	http.HandleFunc("/users", enableCors(handleListUsers))
	http.HandleFunc("/games", enableCors(handleListGames))

	log.Println("Starting the server on port 8080...")
	log.Fatal(http.ListenAndServe(":8080", nil))
}

// Utilities

func enableCors(handler http.HandlerFunc) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		origin := r.Header.Get("Origin")
		if strings.HasPrefix(origin, "http://localhost") || origin == "" {
			w.Header().Set("Access-Control-Allow-Origin", origin)
			w.Header().Set("Access-Control-Allow-Methods", "POST, GET, OPTIONS, PUT, DELETE")
			w.Header().Set("Access-Control-Allow-Headers", "Accept, Content-Type, Content-Length, Accept-Encoding, X-CSRF-Token, Authorization")

			if r.Method == "OPTIONS" {
				w.WriteHeader(http.StatusOK)
				return
			}

			handler(w, r)
		} else {
			log.Printf("CORS origin not allowed: %s", origin)
			http.Error(w, "CORS origin not allowed", http.StatusForbidden)
		}
	}
}

func writeJSONResponse(w http.ResponseWriter, response interface{}) {
	jsonResponse, err := json.MarshalIndent(response, "", "  ")
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	log.Printf("Sending JSON response:\n%s%s%s", Cyan, string(jsonResponse), Reset)
	w.Header().Set("Content-Type", "application/json")
	w.Write(jsonResponse)
}

// User authentication and game creation

func handleUser(w http.ResponseWriter, r *http.Request, userFunc func(*UserRequest) (int, error)) {
	var userReq UserRequest
	err := json.NewDecoder(r.Body).Decode(&userReq)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		log.Printf("Error decoding JSON: %v", err)
		return
	}

	userID, err := userFunc(&userReq)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		log.Printf("Error getting userID: %v", err)
		return
	}

	token, err := addNewTokenToUser(userID)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		log.Printf("Error adding new token to user: %v", err)
		return
	}

	writeJSONResponse(w, map[string]interface{}{"token": token})
}

func authenticateUserHandler(w http.ResponseWriter, r *http.Request) {
	handleUser(w, r, authenticateUser)
}

func registerUserHandler(w http.ResponseWriter, r *http.Request) {
	handleUser(w, r, registerUser)
}

func changePasswordHandler(w http.ResponseWriter, r *http.Request) {
	handleUser(w, r, changePassword)
}

func createNewGameHandler(w http.ResponseWriter, r *http.Request) {
	// extract NewGameRequest from request body
	var request NewGameRequest
	err := json.NewDecoder(r.Body).Decode(&request)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	// create new game
	newGame, err := createGame(request)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	writeJSONResponse(w, newGame)
}

// WebSockets

type Conn struct {
	*websocket.Conn
}

func (c Conn) String() string {
	return fmt.Sprintf("%s%p%s", Blue, c.Conn, Reset)
}

var (
	connectedUsers   = make(map[int][]Conn)
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

type WebSocketMessage struct {
	GameID  int    `json:"game_id"`
	Token   Token  `json:"token"`
	Type    string `json:"message_type,omitempty"`
	Message string `json:"message,omitempty"`
}

func handleWebSocket(w http.ResponseWriter, r *http.Request) {
	log.Println("Handling websocket connection")
	c, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		log.Printf("Failed to upgrade the connection: %v", err)
		return
	}
	conn := Conn{c}
	log.Printf("Upgraded connection %s", conn)
	go listenForWebSocketMessages(conn)
}

func listenForWebSocketMessages(conn Conn) {
	defer conn.Close()

	for {
		messageType, messageData, err := conn.ReadMessage()
		if err != nil {
			log.Printf("Error reading message: %v", err)
			return
		}
		log.Printf("Received message from %s: %s", conn, messageData)

		switch messageType {
		case websocket.TextMessage:
			var message WebSocketMessage
			err := json.Unmarshal(messageData, &message)
			if err != nil {
				log.Printf("Error unmarshalling message for %s: %v", conn, err)
				return
			}
			playerType, token := validateGameToken(message.GameID, message.Token)
			if playerType == InvalidPlayer {
				log.Printf("Invalid game id or token for %s: %d %s", conn, message.GameID, message.Token)
				return
			}
			processMessage(conn, message, playerType, token)
		case websocket.BinaryMessage:
			log.Printf("Error: received non-supported binary message %s", messageData)
			return
		}
	}
}

func processMessage(conn Conn, message WebSocketMessage, playerType PlayerType, token Token) {
	log.Printf("Processing message from %v: %v", conn, message)
	switch message.Type {
	case "Join":
		log.Printf("Player %s joined game %d with token %s", playerType, message.GameID, message.Token)
		game, err := getGame(message.GameID)
		if handleError(conn, message.GameID, err) {
			return
		}
		actions, err := getAllActions(message.GameID)
		if handleError(conn, message.GameID, err) {
			return
		}
		addConnection(message.GameID, conn)
		sendJSONMessage(conn, message.GameID, "GameJoined", map[string]interface{}{
			"player":       playerType.String(),
			"game_token":   token,
			"white_player": game.WhiteUser,
			"black_player": game.BlackUser,
			"actions":      actions,
		})

	case "Action":
		var action Action
		err := json.Unmarshal([]byte(message.Message), &action)
		if err != nil {
			log.Printf("Error unmarshalling action message: %v", err)
			return
		}
		if handleError(conn, message.GameID, checkGameStatus(message.GameID)) {
			log.Printf("Game %d is not in progress", message.GameID)
			return
		}
		if handleError(conn, message.GameID, checkActionValidity(message.GameID, action.ActionNum)) {
			log.Printf("Invalid action number %d for game %d", action.ActionNum, message.GameID)
			return
		}
		// Save the action to the database
		if err := saveAction(message.GameID, action.ActionNum, action.Action, action.Signature); handleError(conn, message.GameID, err) {
			log.Printf("Error saving action: %v", err)
			return
		}
		broadcast(message.GameID, message)

	case "SendFullGame":
		if allActions, err := getAllActions(message.GameID); handleError(conn, message.GameID, err) {
			return
		} else {
			sendJSONMessage(conn, message.GameID, "FullGame", allActions)
		}

	case "RejectAction":
		broadcast(message.GameID, WebSocketMessage{GameID: message.GameID, Type: "GameOver", Message: "Rejected action"})
		if err := markGameAsFinished(message.GameID, "Rejected action detected"); err != nil {
			log.Printf("Error marking game as finished: %v", err)
		}
		return

	case "GameOver":
		broadcast(message.GameID, WebSocketMessage{GameID: message.GameID, Type: "GameOver", Message: message.Message})
		if err := markGameAsFinished(message.GameID, message.Message); err != nil {
			log.Printf("Error marking game as finished: %v", err)
		}
	}
}

func addConnection(gameID int, conn Conn) {
	connectedUsersMu.Lock()
	connectedUsers[gameID] = append(connectedUsers[gameID], conn)
	connectedUsersMu.Unlock()
}

// handleError checks if there is an error and sends an appropriate JSON message. Returns true if there was an error.
func handleError(conn Conn, gameID int, err error) bool {
	if err != nil {
		log.Printf("Error: %v", err)
		sendJSONMessage(conn, gameID, "Error", err.Error())
		return true
	}
	return false
}

func sendJSONMessage(conn Conn, gameId int, messageType string, data any) error {
	prettyJson, err := json.MarshalIndent(data, "", "  ")
	if err != nil {
		log.Printf("Error marshalling JSON: %v", err)
		return err
	}
	log.Printf("Sending JSON message to conn=%s:\n%s%s%s", conn, Cyan, prettyJson, Reset)
	err = conn.WriteJSON(WebSocketMessage{GameID: gameId, Type: messageType, Message: string(prettyJson)})
	if err != nil {
		log.Printf("Error sending JSON message: %v", err)
		return err
	}
	return nil
}

func broadcast(gameID int, action WebSocketMessage) {
	log.Printf("Broadcasting action %v to game %d", action, gameID)
	connectedUsersMu.Lock()
	defer connectedUsersMu.Unlock()

	var activeConnections []Conn

	for _, conn := range connectedUsers[gameID] {
		log.Printf("Sending action to conn %s", conn)
		err := conn.WriteJSON(action)
		if err != nil {
			log.Printf("Failed to send action to conn %s: %v", conn, err)
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

// Functions for server administration

func handleListUsers(w http.ResponseWriter, r *http.Request) {
	users, err := listUsers()
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	writeJSONResponse(w, users)
}

func handleListGames(w http.ResponseWriter, r *http.Request) {
	games, err := listGames()
	log.Printf("Games: %v", games)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	writeJSONResponse(w, games)
}
