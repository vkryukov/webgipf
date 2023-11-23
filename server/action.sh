#!/bin/bash

# Check if the correct number of parameters were provided
if [ "$#" -ne 5 ]; then
    echo "Usage: $0 game_id token action action_number next_action_owner"
    exit 1
fi

# Assign the parameters to variables
game_id=$1
token=$2
action=$3
action_number=$4
next_action_owner=$5

# Create the JSON payload
json_payload=$(printf '{"game_id": %d, "token": "%s", "action": "%s", "action_number": %d, "next_action_owner": %d}' $game_id $token $action $action_number $next_action_owner)

# Send the POST request
curl -X POST -H "Content-Type: application/json" -d "$json_payload" http://localhost:8080/action