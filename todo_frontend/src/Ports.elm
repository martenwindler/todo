port module Ports exposing (saveTasks, saveTags)
import Json.Encode as Encode

port saveTasks : Encode.Value -> Cmd msg
port saveTags : Encode.Value -> Cmd msg