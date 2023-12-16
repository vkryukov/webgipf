// auth provides authentication and registration services.

package main

import (
	"bytes"
	"crypto/rand"
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"text/template"

	"golang.org/x/crypto/bcrypt"
)

func RegisterAuthHandlers() {
	http.HandleFunc("/auth/login", enableCors(loginHandler))
	http.HandleFunc("/auth/check", enableCors(checkHandler))
	http.HandleFunc("/auth/verify", enableCors(verificationHandler))
	http.HandleFunc("/auth/register", enableCors(registerUserHandler))
	http.HandleFunc("/auth/changepassword", enableCors(changePasswordHandler))

	// We need to implement the following endpoints:
	// TODO: resend the verification email
	// TODO: reset the password
	// TODO: change the email address
}

// Tokens

type Token string

func generateToken() Token {
	b := make([]byte, 16)
	rand.Read(b)
	return Token(fmt.Sprintf("%x", b))
}

// Users

type User struct {
	Id            int    `json:"id,omitempty"`
	Username      string `json:"username"`
	Email         string `json:"email"`
	EmailVerified bool   `json:"email_verified"`
	Password      string `json:"password,omitempty"`
	NewPassword   string `json:"new_password,omitempty"`
	CreationTime  int    `json:"creation_time"`
	Token         Token  `json:"token"`
}

func getUserWithToken(token Token) (*User, error) {
	// TODO: differentiate between a token not found and a general error.
	var user User
	err := db.QueryRow(
		`SELECT users.id, users.username, users.email, users.email_verified, users.password, users.creation_time 
		FROM tokens 
		JOIN users ON tokens.user_id = users.id 
		WHERE tokens.token = ?`,
		token).Scan(&user.Id, &user.Username, &user.Email, &user.EmailVerified, &user.Password, &user.CreationTime)
	if err != nil {
		return nil, err
	}
	return &user, nil
}

func getUserWithUsername(username string) (*User, error) {
	// TODO: differentiate between a user not found and a general error.
	var user User
	err := db.QueryRow(
		`SELECT id, username, email, email_verified, password, creation_time 
		FROM users 
		WHERE username = ?`,
		username).Scan(&user.Id, &user.Username, &user.Email, &user.EmailVerified, &user.Password, &user.CreationTime)
	if err != nil {
		return nil, err
	}
	return &user, nil
}

func addNewTokenToUser(userID int) (Token, error) {
	token := generateToken()
	_, err := db.Exec("INSERT INTO tokens(user_id, token) VALUES(?, ?)", userID, token)
	return token, err
}

func authenticateUser(userReq *User) (*User, error) {
	user, err := getUserWithUsername(userReq.Username)
	if err != nil {
		return nil, fmt.Errorf("user %s not found", userReq.Username)
	}
	if !comparePasswords(user.Password, userReq.Password) {
		return nil, fmt.Errorf("wrong password for user %s", userReq.Username)
	}
	return user, nil
}

func comparePasswords(hashedPwd string, plainPwd string) bool {
	return bcrypt.CompareHashAndPassword([]byte(hashedPwd), []byte(plainPwd)) == nil
}

func usernameExists(username string) bool {
	_, err := getUserWithUsername(username)
	return err == nil
}

// serverError logs the detailed error and returns an error message to the client.
func serverError(message string, err error) error {
	log.Printf("Server error %s: %v", message, err)
	return fmt.Errorf("server: " + message)
}

func registerUser(userReq *User) (*User, error) {
	if userReq.Username == "" {
		return nil, fmt.Errorf("missing username")
	}
	if userReq.Email == "" {
		return nil, fmt.Errorf("missing email")
	}
	if userReq.Password == "" {
		return nil, fmt.Errorf("missing password")
	}
	if userReq.Password != userReq.NewPassword {
		return nil, fmt.Errorf("passwords do not match")
	}
	hashedPwd, err := bcrypt.GenerateFromPassword([]byte(userReq.Password), bcrypt.DefaultCost)
	if err != nil {
		return nil, serverError("cannot hash password", err)
	}
	if usernameExists(userReq.Username) {
		return nil, fmt.Errorf("username %s already exists", userReq.Username)
	}
	tx, err := db.Begin()
	if err != nil {
		return nil, serverError("cannot start transaction", err)
	}
	res, err := tx.Exec("INSERT INTO users(username, password, email) VALUES(?, ?, ?)", userReq.Username, hashedPwd, userReq.Email)
	if err != nil {
		tx.Rollback()
		return nil, serverError("cannot insert user", err)
	}
	userID, err := res.LastInsertId()
	if err != nil {
		tx.Rollback()
		return nil, serverError("cannot get last insert ID", err)
	}
	verificationLink, err := createVerificationLink(userID)
	if err != nil {
		tx.Rollback()
		return nil, serverError("cannot create verification link", err)
	}
	err = sendRegistrationEmail(userReq.Username, userReq.Email, verificationLink)
	if err != nil {
		tx.Rollback()
		return nil, serverError("cannot send registration email; check email address", err)
	}
	err = tx.Commit()
	if err != nil {
		return nil, serverError("cannot commit transaction", err)
	}
	return &User{
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

func sendRegistrationEmail(username, email, verificationLink string) error {
	var buf bytes.Buffer
	if err := emailTmpl.Execute(&buf, struct {
		Username         string
		Email            string
		VerificationLink string
	}{username, email, verificationLink}); err != nil {
		return fmt.Errorf("executing email template: %v", err)
	}
	return sendMessage(email, "Gipf Game Server Registration", buf.String())
}

func changePassword(userReq *User) (*User, error) {
	user, err := authenticateUser(userReq)
	if err != nil {
		return nil, err
	}
	newHashPwd, err := bcrypt.GenerateFromPassword([]byte(userReq.NewPassword), bcrypt.DefaultCost)
	if err != nil {
		return nil, serverError("cannot hash password", err)
	}
	tx, err := db.Begin()
	if err != nil {
		return nil, serverError("cannot start transaction", err)
	}
	_, err = tx.Exec("DELETE FROM tokens WHERE user_id = ?", user.Id)
	if err != nil {
		tx.Rollback()
		return nil, serverError("cannot delete old tokens", err)
	}
	_, err = tx.Exec("UPDATE users SET password = ? WHERE id = ?", newHashPwd, user.Id)
	if err != nil {
		tx.Rollback()
		return nil, serverError("cannot update password", err)
	}
	err = tx.Commit()
	if err != nil {
		return nil, serverError("cannot commit transaction", err)
	}
	return user, nil
}

// HTTP Handlers

func sendUserResponse(w http.ResponseWriter, user *User) {
	// We need to send the user information back but hide all the sensitive information.
	user.Id = 0
	user.Password = ""
	user.NewPassword = ""
	writeJSONResponse(w, user)
}

func sendError(w http.ResponseWriter, err error) {
	log.Printf("Sending error to client: %v", err)
	writeJSONResponse(w, struct {
		Error string `json:"error"`
	}{err.Error()})
}

func handleUser(w http.ResponseWriter, r *http.Request, userFunc func(*User) (*User, error)) {
	var userReq User
	err := json.NewDecoder(r.Body).Decode(&userReq)
	if err != nil {
		sendError(w, err)
		return
	}

	user, err := userFunc(&userReq)
	if err != nil {
		sendError(w, err)
		return
	}

	token, err := addNewTokenToUser(user.Id)
	if err != nil {
		sendError(w, err)
		return
	}

	user.Token = token
	sendUserResponse(w, user)
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

func authenticateToken(r *http.Request) (*User, error) {
	token := Token(r.Header.Get("token"))
	if token == "" {
		return nil, fmt.Errorf("missing token")
	}
	return getUserWithToken(token)
}

func verificationHandler(w http.ResponseWriter, r *http.Request) {
	user, err := authenticateToken(r)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}
	_, err = db.Exec("UPDATE users SET verified = 1 WHERE username = ?", user.Username)
	if err != nil {
		sendError(w, err)
		return
	}
	// TODO: indicate the verification is successful
	http.Redirect(w, r, "/", http.StatusSeeOther)
}

func checkHandler(w http.ResponseWriter, r *http.Request) {
	user, err := authenticateToken(r)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}
	sendUserResponse(w, user)
}
