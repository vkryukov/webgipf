package main

import (
	"crypto/tls"
	"encoding/json"
	"flag"
	"log"
	"os"

	"gopkg.in/mail.v2"
)

type Config struct {
	Email    string `json:"email"`
	Password string `json:"password"`
}

func main() {
	// Parsing command line argument for the recipient email address
	recipient := flag.String("to", "", "Recipient email address")
	flag.Parse()

	if *recipient == "" {
		log.Fatal("Recipient email address is required")
	}

	configFile, err := os.ReadFile("config.json")
	if err != nil {
		log.Fatal(err)
	}

	var config Config
	err = json.Unmarshal(configFile, &config)
	if err != nil {
		log.Fatal(err)
	}

	m := mail.NewMessage()
	m.SetHeader("From", config.Email)
	m.SetHeader("To", *recipient)
	m.SetHeader("Subject", "Your Subject Here")
	m.SetBody("text/plain", "This is the email body.")

	d := mail.NewDialer("smtp.fastmail.com", 465, config.Email, config.Password)
	d.TLSConfig = &tls.Config{InsecureSkipVerify: true}

	if err := d.DialAndSend(m); err != nil {
		log.Fatal(err)
	}
}
