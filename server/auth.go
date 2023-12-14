// auth provides authentication and registration services.

package main

import (
	"bytes"
	"context"
	"crypto/rand"
	"encoding/json"
	"fmt"
	"html/template"
	"log"
	"net/http"
	"time"

	"golang.org/x/crypto/bcrypt"
)

func RegisterAuthHandlers() {
	http.HandleFunc("/auth/register", enableCors(registerUserHandler))
	http.HandleFunc("/auth/verify", enableCors(verificationHandler))
	http.HandleFunc("/auth/login", enableCors(loginHandler))
	http.HandleFunc("/auth/changepassword", enableCors(changePasswordHandler))
	http.Handle("/auth/check", AuthMiddleware(enableCors(checkAuthHandler)))
}

func AuthMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		sessionCookie, err := r.Cookie("persistent_session_token")
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

		ctx := context.WithValue(r.Context(), "username", username)
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

type UserResponse struct {
	Id            int    `json:"id,omitempty"`
	Username      string `json:"username"`
	Email         string `json:"email"`
	EmailVerified bool   `json:"email_verified"`
	Token         string `json:"token"`
}

func handleUser(w http.ResponseWriter, r *http.Request, userFunc func(*UserRequest) (*UserResponse, error)) {
	var userReq UserRequest
	err := json.NewDecoder(r.Body).Decode(&userReq)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		log.Printf("Error decoding JSON: %v", err)
		return
	}

	user, err := userFunc(&userReq)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		log.Printf("Error getting user from request: %v", err)
		return
	}

	token, err := addNewTokenToUser(user.Id)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		log.Printf("Error adding new token to user: %v", err)
		return
	}

	user.Token = string(token)
	writeJSONResponse(w, user)
}

func setPersistentSessionCookie(w http.ResponseWriter, token Token) {
	expirationTime := time.Now().AddDate(1, 0, 0) // 1 year from now
	http.SetCookie(w, &http.Cookie{
		Name:     "persistent_session_token",
		Value:    string(token),
		Expires:  expirationTime,
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

func authenticateUser(userReq *UserRequest) (*UserResponse, error) {
	var hashedPwd string
	var user UserResponse

	err := db.QueryRow("SELECT id, password, email, email_verified FROM users WHERE username = ?", userReq.Username).Scan(&user.Id, &hashedPwd, &user.Email, &user.EmailVerified)
	if err != nil {
		return nil, err
	}

	if !comparePasswords(hashedPwd, userReq.Password) {
		return nil, fmt.Errorf("wrong password")
	}

	user.Username = userReq.Username
	return &user, nil
}

var emailTmpl *template.Template

func init() {
	const emailTemplate = `
    Hello Gipf player,

    Thank you for registering for our game server! Here are the details 
    that we have recorded:
        - your username is {{.Username}}
        - your email is {{.Email}}

    IMPORTANT: Your email address is used to reset your password, and 
    needs to be verified. Please click on the following link to verify it:

    {{.VerificationLink}}

    If you did not register for our game server, please ignore this email.

    Regards,
    The Gipf Game Master.
    `

	emailTmpl = template.Must(template.New("email").Parse(emailTemplate))
}

func registerUser(userReq *UserRequest) (*UserResponse, error) {
	hashedPwd, err := bcrypt.GenerateFromPassword([]byte(userReq.Password), bcrypt.DefaultCost)
	if err != nil {
		return nil, err
	}

	res, err := db.Exec("INSERT INTO users(username, password, email) VALUES(?, ?, ?)", userReq.Username, hashedPwd, userReq.Email)
	if err != nil {
		log.Printf("Error inserting user %s into database: %v", userReq.Username, err)
		return nil, err
	}

	userID, err := res.LastInsertId()
	if err != nil {
		log.Printf("Error getting last insert ID: %v", err)
		return nil, err
	}

	verificationLink, err := createVerificationLink(userID)
	if err != nil {
		log.Printf("Error creating verification link: %v", err)
		return nil, err
	}

	sendRegistrationEmail(userReq.Username, userReq.Email, verificationLink)
	return &UserResponse{
		Id:            int(userID),
		Username:      userReq.Username,
		Email:         userReq.Email,
		EmailVerified: false,
	}, nil
}

func createVerificationLink(userID int64) (string, error) {
	token, err := addNewTokenToUser(int(userID))
	if err != nil {
		return "", err
	}

	return fmt.Sprintf("%s/auth/verify?token=%s", baseURL, token), nil
}

func sendRegistrationEmail(username, email, verificationLink string) {
	var buf bytes.Buffer
	if err := emailTmpl.Execute(&buf, struct {
		Username         string
		Email            string
		VerificationLink string
	}{username, email, verificationLink}); err != nil {
		log.Printf("Error executing email template: %v", err)
	}

	err := sendMessage(email, "Gipf Game Server Registration", buf.String())
	if err != nil {
		log.Printf("Error sending registration email to %s: %v", email, err)
	}
}

func verificationHandler(w http.ResponseWriter, r *http.Request) {
	token := r.URL.Query().Get("token")
	if token == "" {
		http.Error(w, "missing token", http.StatusBadRequest)
		return
	}

	username, err := checkUserToken(Token(token))
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	_, err = db.Exec("UPDATE users SET verified = 1 WHERE username = ?", username)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	setPersistentSessionCookie(w, Token(token))
	http.Redirect(w, r, "/", http.StatusSeeOther)
}

func changePassword(userReq *UserRequest) (*UserResponse, error) {
	var hashedPwd string
	var user UserResponse

	err := db.QueryRow("SELECT id, password, email, emailVerified FROM users WHERE username = ?", userReq.Username).Scan(&user.Id, &hashedPwd, &user.Email, &user.EmailVerified)
	if err != nil {
		return nil, err
	}

	if !comparePasswords(hashedPwd, userReq.Password) {
		return nil, fmt.Errorf("wrong password")
	}

	newHashPwd, err := bcrypt.GenerateFromPassword([]byte(userReq.NewPassword), bcrypt.DefaultCost)
	if err != nil {
		return nil, err
	}

	tx, err := db.Begin()
	if err != nil {
		return nil, err
	}

	_, err = tx.Exec("DELETE FROM tokens WHERE user_id = ?", user.Id)
	if err != nil {
		tx.Rollback()
		return nil, err
	}

	_, err = tx.Exec("UPDATE users SET password = ? WHERE id = ?", newHashPwd, user.Id)
	if err != nil {
		tx.Rollback()
		return nil, err
	}

	err = tx.Commit()
	if err != nil {
		return nil, err
	}

	user.Username = userReq.Username
	return &user, nil
}

func checkAuthHandler(w http.ResponseWriter, r *http.Request) {
	username := getUserFromContext(r.Context())
	if username == "" {
		http.Error(w, "no username found", http.StatusInternalServerError)
		log.Printf("Error getting username from context that sits behind auth middleware")
		return
	}
	user, err := getUserFromUsername(username)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		log.Printf("Error getting user from username %s: %v", username, err)
		return
	}
	writeJSONResponse(w, user)
}

type User struct {
	ID            int      `json:"id,omitempty"`
	Username      string   `json:"username"`
	Email         string   `json:"email"`
	EmailVerified bool     `json:"email_verified"`
	CreationTime  int      `json:"creation_time"`
	Tokens        []string `json:"tokens,omitempty"`
}

func getUserFromUsername(username string) (*User, error) {
	var user User

	err := db.QueryRow(
		"SELECT username, email, verified, creation_time FROM users WHERE username = ?",
		username).Scan(&user.Username, &user.Email, &user.EmailVerified, &user.CreationTime)
	if err != nil {
		return nil, err
	}

	return &user, nil
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
