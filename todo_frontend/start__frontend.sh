#!/bin/bash

# Pfad-Logik: Sicherstellen, dass wir im richtigen Verzeichnis operieren
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# In das aktuelle Projektverzeichnis wechseln (angepasst an stopwatch_frontend)
cd stopwatch_frontend

echo -e "\033[1;33m>>> Stopwatch SPA Arbeitsverzeichnis: $(pwd)\033[0m"

# Abhängigkeiten prüfen
if [ ! -d "node_modules" ]; then
    echo -e "\033[1;34m>>> node_modules fehlen. Installiere Abhängigkeiten...\033[0m"
    npm install
fi

# Bereinigung beim Beenden sicherstellen
cleanup() {
    echo -e "\n\033[1;31m>>> Beende Vite Dev-Server und Tests...\033[0m"
    # Beende den Hintergrund-Prozess von Vite
    if [ ! -z "$VITE_PID" ]; then
        kill $VITE_PID 2>/dev/null
    fi
    exit
}

# Wenn das Skript abgebrochen wird (Ctrl+C), cleanup ausführen
trap cleanup SIGINT SIGTERM

echo -e "\033[1;32m>>> Starte Vite (Stopwatch SPA) und Elm-Tests parallel...\033[0m"

# 1. Vite im Hintergrund starten
# Wir leiten die Ausgabe evtl. nicht um, damit du Logs siehst, 
# oder du nutzt 'npm run dev -- --silent' falls es zu voll wird.
npm run dev &
VITE_PID=$!

# 2. Elm-Test im Vordergrund starten (Watch-Mode)
# Das hält das Terminal aktiv und reagiert auf Code-Änderungen
npx elm-test --watch