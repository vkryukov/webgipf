package main

import (
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
	defer db.Close()

	// HTTP handlers
	http.HandleFunc("/authenticate", enableCors(authenticateUserHandler))
	http.HandleFunc("/register", enableCors(registerUserHandler))
	http.HandleFunc("/newgame", enableCors(createNewGameHandler))

	// WebSocket handlers
	http.HandleFunc("/game", enableCors(handleGame))

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
			log.Printf("CORS origin allowed: %s\n", origin)
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

func handleUser(w http.ResponseWriter, r *http.Request, userFunc func(string, string) (int, error)) {
	params := r.URL.Query()
	username := params.Get("username")
	password := params.Get("password")

	userID, err := userFunc(username, password)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	token, err := addNewTokenToUser(userID)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
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

func handleGame(w http.ResponseWriter, r *http.Request) {
	var message WebSocketMessage
	err := json.NewDecoder(r.Body).Decode(&message)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}
	// Check that the game id and token are valid
	playerType, _ := validateGameToken(message.GameID, message.Token)
	if playerType == InvalidPlayer {
		http.Error(w, "Invalid game id or token", http.StatusBadRequest)
		return
	}

	// Upgrade the connection to a WebSocket
	conn, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		log.Printf("Failed to upgrade the connection: %v", err)
		return
	}

	// Add the WebSocket connection to the connected users
	addConnection(message.GameID, conn)

	// Start listening for messages on this connection
	go listenForWebSocketMessages(conn)
}

func addConnection(gameID int, conn *websocket.Conn) {
	connectedUsersMu.Lock()
	defer connectedUsersMu.Unlock()

	// You might want to use a different identifier for each connection
	connectedUsers[gameID] = append(connectedUsers[gameID], conn)
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
			// Handle text message
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
	case "UpgradeToken":
		// We send the game token to the client so that it can be used to sign moves
		if playerType == WhitePlayer {
			sendJSONMessage(conn, WebSocketMessage{GameID: message.GameID, Token: token, Type: "UpgradeToken", Message: "White"})
		} else if playerType == BlackPlayer {
			sendJSONMessage(conn, WebSocketMessage{GameID: message.GameID, Token: token, Type: "UpgradeToken", Message: "Black"})
		} else if playerType == Viewer {
			// there is never a need to upgrade a viewer token, but we send it anyway
			sendJSONMessage(conn, WebSocketMessage{GameID: message.GameID, Token: token, Type: "UpgradeToken", Message: "Viewer"})
		}
	case "Move":
		// Check if the move number is correct
		numMoves, err := getNumberOfMoves(message.GameID)
		if err != nil {
			log.Printf("Error getting number of moves for the game %d: %v", message.GameID, err)
			sendJSONMessage(conn, WebSocketMessage{GameID: message.GameID, Type: "Error", Message: "Error getting number of moves for the game"})
			return
		}
		if message.MoveNum != numMoves+1 {
			log.Printf("Invalid move number: %d, expected %d", message.MoveNum, numMoves+1)
			sendJSONMessage(conn, WebSocketMessage{GameID: message.GameID, Type: "Error",
				Message: fmt.Sprintf("Invalid move number: got %d, expected %d", message.MoveNum, numMoves+1)})
			return
		}
		broadcast(message.GameID, WebSocketMessage{GameID: message.GameID, Type: "Move", Message: message.Message, MoveNum: message.MoveNum, Signature: message.Signature})
	case "SendFullGame":
		allMoves, err := getAllMoves(message.GameID)
		if err != nil {
			log.Printf("Error getting all moves for the game %d: %v", message.GameID, err)
			sendJSONMessage(conn, WebSocketMessage{GameID: message.GameID, Type: "Error", Message: "Error getting all moves for the game"})
			return
		}
		sendJSONMessage(conn, WebSocketMessage{GameID: message.GameID, Type: "FullGame", Message: allMoves})
	}
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

	jsonResponse, err := json.Marshal(users)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	w.Write(jsonResponse)
}
