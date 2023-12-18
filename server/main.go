package main

import (
	"embed"
	"fmt"
	"io"
	"io/fs"
	"log"
	"net/http"
	"os"
	"time"

	"github.com/vkryukov/gameserver"
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

var baseURL string

func init() {
	baseURL = "http://playgipf.com" // TODO: Enable HTTPS
	if os.Getenv("APP_ENV") == "development" {
		baseURL = "http://localhost:8080"
	}
}

//go:embed static/*
var staticFiles embed.FS

func main() {
	log.SetFlags(0)
	log.SetOutput(&customWriter{logFile: os.Stdout})

	gameserver.InitDB("./games.db")
	defer gameserver.CloseDB()

	// Game management

	gameserver.RegisterAuthHandlers("/auth", baseURL)
	gameserver.RegisterGameHandlers("/game", baseURL)
	gameserver.RegisterAdminHandlers("/admin", baseURL)

	fileServer := http.FileServer(http.FS(staticFiles))
	http.Handle("/static/", fileServer)

	fs.WalkDir(staticFiles, ".", func(path string, d fs.DirEntry, err error) error {
		fmt.Println(path)
		return nil
	})

	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path != "/" {
			http.NotFound(w, r)
			return
		}
		// Serve index.html for the root path
		data, err := staticFiles.ReadFile("static/index.html")
		if err != nil {
			http.Error(w, "Internal Server Error", http.StatusInternalServerError)
			return
		}
		w.Write(data)
	})

	log.Println("Starting the server on port 8080...")
	log.Fatal(http.ListenAndServe(":8080", nil))
}
