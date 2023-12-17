package main

import "testing"

// Tests
func TestAuth(t *testing.T) {
	initDB(":memory:")

	userReq := &User{Email: "test@example.com", Password: "password"}

	// Test 1: after registering a user, it can be found with getUserWithToken and getUserWithEmail
	registeredUser, err := registerUser(userReq)
	if err != nil {
		t.Fatalf("Failed to register user: %v", err)
	}

	foundUser, err := getUserWithEmail(userReq.Email)
	if err != nil || foundUser.Email != registeredUser.Email {
		t.Fatalf("Failed to find user with getUserWithEmail: %v", err)
	}

	foundUser, err = getUserWithToken(registeredUser.Token)
	if err != nil || foundUser.Email != registeredUser.Email {
		t.Fatalf("Failed to find user with getUserWithToken: %v", err)
	}

	// Test 2: after registering a user, emailExists should return true.
	if !emailExists(userReq.Email) {
		t.Fatalf("emailExists returned false after registering user")
	}

	// Test 3: after registering a user, another one cannot be registered with the same email.
	_, err = registerUser(userReq)
	if err == nil {
		t.Fatalf("Expected error when registering user with duplicate email, got nil")
	}
}
