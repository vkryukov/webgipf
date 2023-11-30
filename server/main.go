package main

import (
	"encoding/json"
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

	// HTTP handlers
	http.HandleFunc("/authenticate", enableCors(authenticateUserHandler))
	http.HandleFunc("/register", enableCors(registerUserHandler))
	http.HandleFunc("/newgame", enableCors(createNewGameHandler))

	// WebSocket handlers
	http.HandleFunc("/game", handleWebSocket)

	// Server administration
	http.HandleFunc("/users", enableCors(handleListUsers))

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
			log.Printf("CORS origin not allowed: %s\n", origin)
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
	w.Header().Set("Content-Type", "application/json")
	w.Write(jsonResponse)
}

// User authentication and game creation

type UserRequest struct {
	Username    string `json:"username"`
	Password    string `json:"password"`
	NewPassword string `json:"new_password,omitempty"`
}

func handleUser(w http.ResponseWriter, r *http.Request, userFunc func(string, string) (int, error)) {
	var userReq UserRequest
	err := json.NewDecoder(r.Body).Decode(&userReq)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		log.Printf("Error decoding JSON: %v", err)
		return
	}

	userID, err := userFunc(userReq.Username, userReq.Password)
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
	GameID    int    `json:"game_id"`
	Token     Token  `json:"token"`
	Type      string `json:"action_type,omitempty"`
	Message   string `json:"action,omitempty"`
	Signature string `json:"signature,omitempty"`
	MoveNum   int    `json:"action_num,omitempty"`
}

func handleWebSocket(w http.ResponseWriter, r *http.Request) {
	conn, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		log.Printf("Failed to upgrade the connection: %v", err)
		return
	}
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

func processMessage(conn *websocket.Conn, message WebSocketMessage, playerType PlayerType, token Token) {
	switch message.Type {
	case "Join":
		addConnection(message.GameID, conn)
		sendJSONMessage(conn, WebSocketMessage{GameID: message.GameID, Type: "UpgradeToken", Message: playerType.String()})

	case "Move":
		if handleError(conn, message.GameID, checkGameStatus(message.GameID)) {
			return
		}
		if handleError(conn, message.GameID, checkMoveValidity(message.GameID, message.MoveNum)) {
			return
		}
		broadcast(message.GameID, WebSocketMessage{GameID: message.GameID, Type: "Move", Message: message.Message, MoveNum: message.MoveNum, Signature: message.Signature})

	case "SendFullGame":
		if allMoves, err := getAllMoves(message.GameID); handleError(conn, message.GameID, err) {
			return
		} else {
			sendJSONMessage(conn, WebSocketMessage{GameID: message.GameID, Type: "FullGame", Message: allMoves})
		}

	case "RejectMove":
		broadcast(message.GameID, WebSocketMessage{GameID: message.GameID, Type: "GameOver", Message: "Rejected move"})
		if err := markGameAsFinished(message.GameID, "Rejected move detected"); err != nil {
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

func sendJSONMessage(conn *websocket.Conn, data interface{}) error {
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
