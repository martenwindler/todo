port module Ports exposing (..)

import Json.Encode as Encode

-- 1. PERSISTENCE PORTS (Elm -> JS)
port saveTasks : Encode.Value -> Cmd msg
port saveTags : Encode.Value -> Cmd msg
port saveInputs : Encode.Value -> Cmd msg

-- 2. DEADLINE PORTS (Elm <-> JS)

-- Sends the ID and the "YYYY-MM-DD" string to JS for parsing
port requestDateTimestamp : { id : Int, dateStr : String } -> Cmd msg

-- Receives the parsed Unix timestamp (milliseconds) back from JS
-- We use a Record here to match the `{ id: number, timestamp: number | null }` from JS
port receiveDateTimestamp : ({ id : Int, timestamp : Maybe Int } -> msg) -> Sub msg