# --- KONFIGURATION ---
SHELL := /bin/bash
START_SCRIPT := ./todo_frontend/start__frontend.sh

.PHONY: help start install build clean

# Standard-Ziel: Hilfe anzeigen
help:
	@echo "Verfügbare Befehle:"
	@echo "  make start   - Startet das Frontend (Vite + Tests im Watch-Mode)"
	@echo "  make install - Installiert npm-Abhängigkeiten in todo_frontend"
	@echo "  make build   - Erstellt den Produktions-Build"
	@echo "  make clean   - Löscht Build-Artefakte und Elm-Stuff"

# Startet das Frontend über dein Shell-Skript
start:
	@chmod +x $(START_SCRIPT)
	@$(START_SCRIPT)

# Bequemlichkeits-Befehl für die Installation
install:
	cd todo_frontend && npm install

# Produktions-Build
build:
	cd todo_frontend && npm run build

# Aufräumen
clean:
	rm -rf todo_frontend/dist
	rm -rf todo_frontend/elm-stuff