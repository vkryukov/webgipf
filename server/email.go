package main

import (
	"crypto/tls"
	"encoding/json"
	"log"
	"os"

	"gopkg.in/mail.v2"
)

type Config struct {
	Email    string `json:"email"`
	Password string `json:"password"`
}

var globalConfig Config

func init() {
	configFile, err := os.ReadFile("config.json")
	if err != nil {
		log.Fatal(err)
	}

	err = json.Unmarshal(configFile, &globalConfig)
	if err != nil {
		log.Fatal(err)
	}
}

func sendMessage(to, subject, body string) error {
	m := mail.NewMessage()
	m.SetHeader("From", globalConfig.Email)
	m.SetHeader("To", to)
	m.SetHeader("Subject", subject)
	m.SetBody("text/plain", body)

	d := mail.NewDialer("smtp.fastmail.com", 465, globalConfig.Email, globalConfig.Password)
	d.TLSConfig = &tls.Config{InsecureSkipVerify: true}

	return d.DialAndSend(m)
}
