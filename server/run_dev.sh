#!/bin/bash
# Compile and run a development server

# Exit script on first error
set -e

# Set environment variables
export APP_ENV=development

# Compile
cd ../client
mkdir -p ../server/static
elm make src/App.elm --output ../server/static/app.js
cp index.html ../server/static/index.html
npx tailwindcss -i tailwind.css -o ../server/static/gipf.css 
cd ../server
go build -o server

./server -noemail
