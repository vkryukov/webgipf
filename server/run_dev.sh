#!/bin/bash
# Compile and run a development server

# Set environment variables
export APP_ENV=development

# Compile
cd ../client
elm make src/App.elm --output ../server/static/app.js
cd ../server
go build -o server

./server
