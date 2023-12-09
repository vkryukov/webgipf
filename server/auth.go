// auth provides authentication and registration services.

package main

import (
	"context"
	"crypto/rand"
	"encoding/json"
	"fmt"
	"log"
	"net/http"

	"golang.org/x/crypto/bcrypt"
)

func RegisterAuthHandlers() {
	http.HandleFunc("/auth/login", enableCors(loginHandler))
	http.HandleFunc("/auth/register", enableCors(registerUserHandler))
	http.HandleFunc("/auth/changepassword", enableCors(changePasswordHandler))
}

func AuthMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		sessionCookie, err := r.Cookie("session_token")
		if err != nil {
			if err == http.ErrNoCookie {
				w.WriteHeader(http.StatusUnauthorized)
				return
			}
			w.WriteHeader(http.StatusBadRequest)
			return
		}

		sessionToken := sessionCookie.Value
		username, err := checkUserToken(Token(sessionToken))
		if err != nil {
			w.WriteHeader(http.StatusUnauthorized)
			return
		}

		ctx := context.WithValue(r.Context(), "usernane", username)
		r = r.WithContext(ctx)
		next.ServeHTTP(w, r)
	})
}

func checkUserToken(token Token) (string, error) {
	var username string

	err := db.QueryRow(
		"SELECT users.username FROM tokens JOIN users ON tokens.user_id = users.id WHERE tokens.token = ?",
		token).Scan(&username)
	if err != nil {
		return "", err
	}

	return username, nil
}

func getUserFromContext(ctx context.Context) string {
	userID, ok := ctx.Value("username").(string)
	if !ok {
		log.Printf("Error getting userID from context")
		return ""
	}

	return userID
}

type UserRequest struct {
	Username    string `json:"username"`
	Password    string `json:"password"`
	Email       string `json:"email,omitempty"`
	NewPassword string `json:"new_password,omitempty"`
}

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

	http.SetCookie(w, &http.Cookie{
		Name:  "session_token",
		Value: string(token),
		// TODO: Set Secure to true when using HTTPS
		// Secure:   true,
		HttpOnly: true,
	})
}

func loginHandler(w http.ResponseWriter, r *http.Request) {
	handleUser(w, r, authenticateUser)
}

func registerUserHandler(w http.ResponseWriter, r *http.Request) {
	handleUser(w, r, registerUser)
}

func changePasswordHandler(w http.ResponseWriter, r *http.Request) {
	handleUser(w, r, changePassword)
}

func comparePasswords(hashedPwd string, plainPwd string) bool {
	return bcrypt.CompareHashAndPassword([]byte(hashedPwd), []byte(plainPwd)) == nil
}

func authenticateUser(userReq *UserRequest) (int, error) {
	var userID int
	var hashedPwd string

	err := db.QueryRow("SELECT id, password FROM users WHERE username = ?", userReq.Username).Scan(&userID, &hashedPwd)
	if err != nil {
		return -1, err
	}

	if !comparePasswords(hashedPwd, userReq.Password) {
		return -1, fmt.Errorf("wrong password")
	}

	return userID, nil
}

func registerUser(userReq *UserRequest) (int, error) {
	hashedPwd, err := bcrypt.GenerateFromPassword([]byte(userReq.Password), bcrypt.DefaultCost)
	if err != nil {
		return -1, err
	}

	res, err := db.Exec("INSERT INTO users(username, password, email) VALUES(?, ?, ?)", userReq.Username, hashedPwd, userReq.Email)
	if err != nil {
		log.Printf("Error inserting user %s into database: %v", userReq.Username, err)
		return -1, err
	}

	userID, err := res.LastInsertId()
	if err != nil {
		return -1, err
	}

	return int(userID), nil
}

func changePassword(userReq *UserRequest) (int, error) {
	var userID int
	var hashedPwd string

	err := db.QueryRow("SELECT id, password FROM users WHERE username = ?", userReq.Username).Scan(&userID, &hashedPwd)
	if err != nil {
		return -1, err
	}

	if !comparePasswords(hashedPwd, userReq.Password) {
		return -1, fmt.Errorf("wrong password")
	}

	newHashPwd, err := bcrypt.GenerateFromPassword([]byte(userReq.NewPassword), bcrypt.DefaultCost)
	if err != nil {
		return -1, err
	}

	tx, err := db.Begin()
	if err != nil {
		return -1, err
	}

	_, err = tx.Exec("DELETE FROM tokens WHERE user_id = ?", userID)
	if err != nil {
		tx.Rollback()
		return -1, err
	}

	_, err = tx.Exec("UPDATE users SET password = ? WHERE id = ?", newHashPwd, userID)
	if err != nil {
		tx.Rollback()
		return -1, err
	}

	err = tx.Commit()
	if err != nil {
		return -1, err
	}

	return userID, nil
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
