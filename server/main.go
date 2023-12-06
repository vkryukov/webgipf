package main

import (
	"crypto/sha1"
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"strings"
	"sync"

	"github.com/gorilla/websocket"
	_ "github.com/mattn/go-sqlite3"
)

func main() {
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
	jsonResponse, err := json.Marshal(response)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	log.Printf("Sending JSON response: %s", jsonResponse)
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

type WebSocketMessage struct {
	GameID  int    `json:"game_id"`
	Token   Token  `json:"token"`
	Type    string `json:"message_type,omitempty"`
	Message string `json:"message,omitempty"`
}

func handleWebSocket(w http.ResponseWriter, r *http.Request) {
	log.Println("Handling websocket connection")
	conn, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		log.Printf("Failed to upgrade the connection: %v", err)
		return
	}
	log.Printf("Upgraded connection %s", connId(conn))
	go listenForWebSocketMessages(conn)
}

func listenForWebSocketMessages(conn *websocket.Conn) {
	defer conn.Close()

	for {
		messageType, messageData, err := conn.ReadMessage()
		if err != nil {
			log.Printf("Error reading message: %v", err)
			return
		}
		log.Printf("Received message from %s: %s", connId(conn), messageData)

		switch messageType {
		case websocket.TextMessage:
			var message WebSocketMessage
			err := json.Unmarshal(messageData, &message)
			if err != nil {
				log.Printf("Error unmarshalling message: %v", err)
				return
			}
			playerType, token := validateGameToken(message.GameID, message.Token)
			if playerType == InvalidPlayer {
				log.Printf("Invalid game id or token: %d %s", message.GameID, message.Token)
				return
			}
			processMessage(conn, message, playerType, token)
		case websocket.BinaryMessage:
			log.Printf("Error: received non-supported binary message %s", messageData)
			return
		}
	}
}

// ActionMessage is encoded as JSON: {"action_num": 1, "action": "i1-h2", signature: "0x1234"}
type ActionMessage struct {
	ActionNum int    `json:"action_num"`
	Action    string `json:"action"`
	Signature string `json:"signature"`
}

func processMessage(conn *websocket.Conn, message WebSocketMessage, playerType PlayerType, token Token) {
	log.Printf("Processing message from %v: %v", connId(conn), message)
	switch message.Type {
	case "Join":
		log.Printf("Player %s joined game %d with token %s", playerType, message.GameID, message.Token)
		addConnection(message.GameID, conn)
		sendJSONMessage(conn, WebSocketMessage{GameID: message.GameID, Type: "UpgradeToken", Message: playerType.String()})

	case "Action":
		var action ActionMessage
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
			sendJSONMessage(conn, WebSocketMessage{GameID: message.GameID, Type: "FullGame", Message: allActions})
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

func addConnection(gameID int, conn *websocket.Conn) {
	connectedUsersMu.Lock()
	connectedUsers[gameID] = append(connectedUsers[gameID], conn)
	connectedUsersMu.Unlock()
}

// handleError checks if there is an error and sends an appropriate JSON message. Returns true if there was an error.
func handleError(conn *websocket.Conn, gameID int, err error) bool {
	if err != nil {
		log.Printf("Error: %v", err)
		sendJSONMessage(conn, WebSocketMessage{GameID: gameID, Type: "Error", Message: err.Error()})
		return true
	}
	return false
}

func connId(conn *websocket.Conn) string {
	hash := sha1.New()
	fmt.Fprintf(hash, "%p", conn)
	return fmt.Sprintf("%x", hash.Sum(nil))
}

func sendJSONMessage(conn *websocket.Conn, data interface{}) error {
	log.Printf("Sending JSON message to %v: %v", connId(conn), data)
	err := conn.WriteJSON(data)
	if err != nil {
		log.Printf("Error sending JSON message: %v", err)
		return err
	}
	return nil
}

func broadcast(gameID int, action WebSocketMessage) {
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
