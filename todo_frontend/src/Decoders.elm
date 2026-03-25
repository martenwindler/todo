module Decoders exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Time

type TaskCategory = Today | Upcoming | Anytime | Someday | Logbook

type Task
    = Task
        { id : Int
        , content : String
        , completed : Bool
        , tags : List String
        , category : TaskCategory
        , subtasks : List Task
        , deadline : Maybe Int 
        }

type alias Tag = { name : String, colour : String }

categoryToString : TaskCategory -> String
categoryToString cat =
    case cat of
        Today -> "Today"
        Upcoming -> "Upcoming"
        Anytime -> "Anytime"
        Someday -> "Someday"
        Logbook -> "Logbook"

encodeTask : Task -> Encode.Value
encodeTask (Task t) =
    Encode.object
        [ ("id", Encode.int t.id)
        , ("content", Encode.string t.content)
        , ("completed", Encode.bool t.completed)
        , ("tags", Encode.list Encode.string t.tags)
        , ("category", Encode.string (categoryToString t.category))
        , ("subtasks", Encode.list encodeTask t.subtasks)
        , ("deadline", case t.deadline of
                            Just ts -> Encode.int ts
                            Nothing -> Encode.null)
        ]

encodeTag : Tag -> Encode.Value
encodeTag tag = Encode.object [ ("name", Encode.string tag.name), ("colour", Encode.string tag.colour) ]

encodeInputs : { input : String, tagInput : String } -> Encode.Value
encodeInputs model = Encode.object [ ("taskInput", Encode.string model.input), ("tagInput", Encode.string model.tagInput) ]

categoryDecoder : Decoder TaskCategory
categoryDecoder =
    Decode.string |> Decode.andThen (\str ->
        case str of
            "Today" -> Decode.succeed Today
            "Upcoming" -> Decode.succeed Upcoming
            "Anytime" -> Decode.succeed Anytime
            "Someday" -> Decode.succeed Someday
            "Logbook" -> Decode.succeed Logbook
            _ -> Decode.succeed Anytime)

taskDecoder : Decoder Task
taskDecoder =
    Decode.map7 (\id_ content completed tags category subtasks deadline -> 
        Task { id = id_, content = content, completed = completed, tags = tags, category = category, subtasks = subtasks, deadline = deadline })
        (Decode.field "id" Decode.int) 
        (Decode.field "content" Decode.string) 
        (Decode.field "completed" Decode.bool)
        (Decode.field "tags" (Decode.list Decode.string)) 
        (Decode.field "category" categoryDecoder)
        (Decode.lazy (\_ -> Decode.field "subtasks" (Decode.list taskDecoder)))
        (Decode.maybe (Decode.field "deadline" Decode.int))

tagDecoder : Decoder Tag
tagDecoder = Decode.map2 Tag (Decode.field "name" Decode.string) (Decode.field "colour" Decode.string)

monthToInt : Time.Month -> Int
monthToInt month =
    case month of
        Time.Jan -> 1
        Time.Feb -> 2
        Time.Mar -> 3
        Time.Apr -> 4
        Time.May -> 5
        Time.Jun -> 6
        Time.Jul -> 7
        Time.Aug -> 8
        Time.Sep -> 9
        Time.Oct -> 10
        Time.Nov -> 11
        Time.Dec -> 12

millisToIsoDateTime : Time.Zone -> Maybe Int -> String
millisToIsoDateTime zone maybeMillis =
    case maybeMillis of
        Just ms ->
            let
                posix = Time.millisToPosix ms
                y = String.fromInt (Time.toYear zone posix)
                m = String.padLeft 2 '0' (String.fromInt (monthToInt (Time.toMonth zone posix)))
                d = String.padLeft 2 '0' (String.fromInt (Time.toDay zone posix))
                hh = String.padLeft 2 '0' (String.fromInt (Time.toHour zone posix))
                mm = String.padLeft 2 '0' (String.fromInt (Time.toMinute zone posix))
            in
            y ++ "-" ++ m ++ "-" ++ d ++ "T" ++ hh ++ ":" ++ mm
        Nothing -> ""