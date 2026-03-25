port module Ports exposing (..)

import Json.Encode as Encode

port saveTasks : Encode.Value -> Cmd msg
port saveTags : Encode.Value -> Cmd msg
port saveInputs : Encode.Value -> Cmd msg

-- Add these two:
port requestDateTimestamp : { id : Int, dateStr : String } -> Cmd msg
port receiveDateTimestamp : ({ id : Int, timestamp : Maybe Int } -> msg) -> Sub msg