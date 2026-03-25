port module Ports exposing (saveTasks, saveTags, saveInputs)

import Json.Encode as Encode

port saveTasks : Encode.Value -> Cmd msg
port saveTags : Encode.Value -> Cmd msg
port saveInputs : Encode.Value -> Cmd msg -- Must be here!