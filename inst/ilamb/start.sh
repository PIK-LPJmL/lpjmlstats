#!/usr/bin/env bash

# This script picks a random port between 4000 and 5000, then starts a simple web server.
# Usage: ./start.sh

PORT=$(( ( RANDOM % 1001 ) + 4000 ))
echo "Starting server on port $PORT..."

cd _build
module load anaconda
python -m http.server "$PORT"
