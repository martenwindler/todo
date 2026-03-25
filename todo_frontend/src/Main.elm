module Main exposing (..)

import Browser
import Html exposing (Html, a, button, div, h1, input, label, li, span, text, ul)
import Html.Attributes as Attr exposing (class, checked, href, placeholder, style, value, autofocus)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit, onBlur)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Ports
import Svg exposing (svg, path)
import Svg.Attributes as SvgAttr

-- 1. MODEL

type Task
    = Task
        { id : Int
        , content : String
        , completed : Bool
        , tags : List String
        , subtasks : List Task
        }

type alias Tag = { name : String, colour : String }
type Filter = All | Active | Completed

type alias Model =
    { tasks : List Task
    , input : String
    , tagInput : String
    , activeFilter : Filter
    , activeTag : String
    , tags : List Tag
    , nextId : Int
    , editingId : Maybe Int 
    , editInput : String    
    }

initialModel : Model
initialModel =
    { tasks = []
    , input = ""
    , tagInput = ""
    , activeFilter = All
    , activeTag = "all"
    , tags = [] 
    , nextId = 1
    , editingId = Nothing
    , editInput = ""
    }

-- 2. JSON ENCODERS (For Saving)

encodeTask : Task -> Encode.Value
encodeTask (Task t) =
    Encode.object
        [ ("id", Encode.int t.id)
        , ("content", Encode.string t.content)
        , ("completed", Encode.bool t.completed)
        , ("tags", Encode.list Encode.string t.tags)
        , ("subtasks", Encode.list encodeTask t.subtasks)
        ]

encodeTag : Tag -> Encode.Value
encodeTag tag =
    Encode.object
        [ ("name", Encode.string tag.name)
        , ("colour", Encode.string tag.colour)
        ]

-- 3. JSON DECODERS (For Loading via Flags)

taskDecoder : Decoder Task
taskDecoder =
    Decode.map5 (\id content completed tags subtasks -> 
        Task { id = id, content = content, completed = completed, tags = tags, subtasks = subtasks })
        (Decode.field "id" Decode.int)
        (Decode.field "content" Decode.string)
        (Decode.field "completed" Decode.bool)
        (Decode.field "tags" (Decode.list Decode.string))
        (Decode.lazy (\_ -> Decode.field "subtasks" (Decode.list taskDecoder)))

tagDecoder : Decoder Tag
tagDecoder =
    Decode.map2 Tag
        (Decode.field "name" Decode.string)
        (Decode.field "colour" Decode.string)

-- 4. UPDATE

type Msg
    = UpdateInput String
    | UpdateTagInput String
    | AddTask
    | AddSubtask Int
    | DeleteTask Int
    | ToggleTask Int
    | MoveRight Int
    | MoveLeft Int
    | SetFilter Filter
    | SetTag String
    | Reset
    | CreateGlobalTag
    | DeleteGlobalTag String
    | ToggleTagOnTask Int String
    | StartEdit Int String
    | UpdateEditInput String
    | SaveEdit
    | CancelEdit
    | ClearCompleted

-- This wrapper triggers the ports every time the model is updated
updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        newModel = update msg model
        
        -- Commands to save to localStorage
        saveTasksCmd = Ports.saveTasks (Encode.list encodeTask newModel.tasks)
        saveTagsCmd = Ports.saveTags (Encode.list encodeTag newModel.tags)
    in
    ( newModel, Cmd.batch [ saveTasksCmd, saveTagsCmd ] )

update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateInput str -> { model | input = str }
        UpdateTagInput str -> { model | tagInput = str }
        StartEdit id content -> { model | editingId = Just id, editInput = content }
        UpdateEditInput str -> { model | editInput = str }
        SaveEdit ->
            case model.editingId of
                Just id -> { model | tasks = updateHelper id (\(Task t) -> Task { t | content = model.editInput }) model.tasks, editingId = Nothing }
                Nothing -> model
        CancelEdit -> { model | editingId = Nothing }
        
        ToggleTask id -> { model | tasks = toggleAndCheckParent id model.tasks }

        AddTask ->
            if String.isEmpty model.input then model else
                let newTask = Task { id = model.nextId, content = model.input, completed = False, tags = [], subtasks = [] }
                in { model | tasks = newTask :: model.tasks, input = "", nextId = model.nextId + 1 }

        AddSubtask pId ->
            let newS = Task { id = model.nextId, content = "New Subtask", completed = False, tags = [], subtasks = [] }
                add (Task t) = Task { t | subtasks = t.subtasks ++ [newS], completed = False }
            in { model | tasks = updateHelper pId add model.tasks, nextId = model.nextId + 1 }

        DeleteTask id -> { model | tasks = deleteHelper id model.tasks }
        MoveRight id -> { model | tasks = indentRight id model.tasks }
        MoveLeft id -> 
            let (updated, extracted) = indentLeft id model.tasks
            in { model | tasks = updated ++ extracted }

        CreateGlobalTag ->
            if String.isEmpty model.tagInput || List.any (\t -> t.name == model.tagInput) model.tags then model else
                { model | tags = model.tags ++ [{ name = model.tagInput, colour = "#5c3df5" }], tagInput = "" }

        DeleteGlobalTag tagName ->
            let strip (Task t) = Task { t | tags = List.filter (\n -> n /= tagName) t.tags, subtasks = List.map strip t.subtasks }
            in { model | tags = List.filter (\t -> t.name /= tagName) model.tags, tasks = List.map strip model.tasks }

        ToggleTagOnTask taskId tagName ->
            let toggle (Task t) = Task { t | tags = if List.member tagName t.tags then List.filter ((/=) tagName) t.tags else tagName :: t.tags }
            in { model | tasks = updateHelper taskId toggle model.tasks }

        SetFilter f -> { model | activeFilter = f }
        SetTag t -> { model | activeTag = t }
        Reset -> initialModel
        ClearCompleted ->
            let filterComp tasks = tasks |> List.filter (\(Task t) -> not t.completed) |> List.map (\(Task t) -> Task { t | subtasks = filterComp t.subtasks })
            in { model | tasks = filterComp model.tasks }

-- (Helpers toggleAndCheckParent, updateHelper, deleteHelper, indentRight, indentLeft, countAll, countDone remain same as your provided code)

toggleAndCheckParent targetId list =
    List.map (\(Task t) ->
            let
                updatedSubtasks = if t.id == targetId then t.subtasks else toggleAndCheckParent targetId t.subtasks
                newCompleted = if t.id == targetId then not t.completed else if not (List.isEmpty updatedSubtasks) then List.all (\(Task sub) -> sub.completed) updatedSubtasks else t.completed
            in Task { t | subtasks = updatedSubtasks, completed = newCompleted }
        ) list

updateHelper id fn list = List.map (\(Task t) -> if t.id == id then fn (Task t) else Task { t | subtasks = updateHelper id fn t.subtasks }) list
deleteHelper id list = list |> List.filter (\(Task t) -> t.id /= id) |> List.map (\(Task t) -> Task { t | subtasks = deleteHelper id t.subtasks })
indentRight targetId list = case list of
    [] -> []
    [only] -> let (Task t) = only in [ Task { t | subtasks = indentRight targetId t.subtasks } ]
    (Task f) :: (Task s) :: r -> if s.id == targetId then [ Task { f | subtasks = f.subtasks ++ [Task s] } ] ++ r else Task { f | subtasks = indentRight targetId f.subtasks } :: indentRight targetId (Task s :: r)
indentLeft targetId list = let folder (Task t) (accL, accE) = if t.id == targetId then (accL, accE ++ [Task t]) else let (newS, ext) = indentLeft targetId t.subtasks in (accL ++ [Task { t | subtasks = newS }], accE ++ ext) in List.foldl folder ([], []) list

countAll list = List.foldl (\(Task t) acc -> 1 + acc + countAll t.subtasks) 0 list
countDone list = List.foldl (\(Task t) acc -> (if t.completed then 1 else 0) + acc + countDone t.subtasks) 0 list

-- 5. VIEW (Remains largely the same as your code)

viewProgress subtasks =
    let
        total = countAll subtasks
        done = countDone subtasks
        percentage = if total == 0 then 0 else (toFloat done / toFloat total) * 100
        degrees = percentage * 3.6
        rightStyle = if percentage >= 50 then [ style "background" "inherit", style "transform" ("rotate(" ++ String.fromFloat (degrees - 180) ++ "deg)") ] else [ style "background" "#ccc", style "transform" ("rotate(" ++ String.fromFloat degrees ++ "deg)") ]
    in
    div [ class "progress-holder", style "width" "34px", style "height" "34px" ]
        [ div [ class "semi-circle" ] [], div [ class "left-block" ] [], div (class "right-block" :: rightStyle) []
        , div [ class "mask", style "width" "76%", style "height" "76%", style "top" "12%", style "left" "12%" ] 
            [ span [ style "font-size" "9px", style "top" "calc(50% - 6px)" ] [ text (String.fromInt (round percentage) ++ "%") ] ]
        ]

viewTaskItem model (Task t) =
    let isEditing = model.editingId == Just t.id in
    li []
        [ div [ class "task-row-container", style "display" "flex", style "align-items" "center", style "gap" "8px" ] 
            [ button [ onClick (MoveLeft t.id), class "nav-btn" ] [ text "‹" ]
            , button [ onClick (MoveRight t.id), class "nav-btn" ] [ text "›" ]
            , input [ Attr.type_ "checkbox", checked t.completed, onCheck (\_ -> ToggleTask t.id) ] []
            , if isEditing then
                input [ value model.editInput, onInput UpdateEditInput, onBlur SaveEdit, onEnter SaveEdit, autofocus True, style "flex-grow" "1", class "edit-input" ] []
              else
                span [ style "flex-grow" "1", class (if t.completed then "task-item-checked" else ""), onClick (StartEdit t.id t.content) ] 
                    [ text t.content, span [ class "task-strike" ] [] ]
            , if not (List.isEmpty t.subtasks) then viewProgress t.subtasks else text ""
            , div [ style "display" "flex", style "gap" "3px" ] 
                (List.map (\tag -> button [ onClick (ToggleTagOnTask t.id tag.name), style "font-size" "10px", style "background" (if List.member tag.name t.tags then "#5c3df5" else "#f0f0f0"), style "color" (if List.member tag.name t.tags then "white" else "#666"), style "border" "none", style "border-radius" "3px" ] [ text tag.name ]) model.tags)
            , button [ class "add-sub-btn", onClick (AddSubtask t.id) ] [ text "+" ]
            , button [ class "task-item-remove", onClick (DeleteTask t.id) ] [ svg [ SvgAttr.viewBox "0 0 40 40" ] [ path [ SvgAttr.d "M15 15 L25 25 M25 15 L15 25" ] [] ] ]
            ]
        , if List.isEmpty t.subtasks then text "" else div [ style "margin-left" "35px", style "border-left" "1px solid #eee" ] [ ul [ class "task-list" ] (List.map (viewTaskItem model) t.subtasks) ]
        ]

onEnter msg = Html.Events.on "keydown" (Decode.field "key" Decode.string |> Decode.andThen (\k -> if k == "Enter" then Decode.succeed msg else Decode.fail ""))
viewForm i = Html.form [ class "task-form", onSubmit AddTask ] [ input [ placeholder "New Task...", value i, onInput UpdateInput ] [], button [ Attr.type_ "submit" ] [ text "Add" ] ]
viewTagManager m = div [ class "task-tags", style "padding" "10px" ] [ div [ style "display" "flex", style "gap" "10px" ] [ input [ placeholder "Tag name", value m.tagInput, onInput UpdateTagInput, style "flex-grow" "1" ] [], button [ onClick CreateGlobalTag ] [ text "Add Tag" ] ], div [ style "display" "flex", style "gap" "5px", style "margin-top" "5px" ] (List.map (\tag -> span [ class "tag-label" ] [ text tag.name, button [ onClick (DeleteGlobalTag tag.name), style "border" "none", style "background" "none", style "color" "red" ] [ text "×" ] ]) m.tags) ]
viewControls m = div [ class "task-controls" ] [ button [ onClick (SetTag "all") ] [ text "All Tags" ], div [ style "display" "inline-block", style "margin-left" "10px" ] (List.map (\tag -> button [ onClick (SetTag tag.name) ] [ text tag.name ]) m.tags), button [ onClick ClearCompleted, style "float" "right" ] [ text "Clear Completed" ] ]

viewRoot model = div [ class "wrap" ] [ h1 [] [ text "Todo" ], div [ class "app" ] [ viewForm model.input, viewTagManager model, ul [ class "task-list" ] (List.map (viewTaskItem model) model.tasks), viewControls model ] ]

-- 6. MAIN & INIT

init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        -- Read tasks from flags
        decodedTasks = 
            Decode.decodeValue (Decode.field "tasks" (Decode.list taskDecoder)) flags
                |> Result.withDefault []

        -- Read tags from flags
        decodedTags = 
            Decode.decodeValue (Decode.field "tags" (Decode.list tagDecoder)) flags
                |> Result.withDefault []

        -- Calculate the nextId based on existing tasks
        maxId = 
            let
                findMaxId list currentMax =
                    List.foldl (\(Task t) m -> findMaxId t.subtasks (max m t.id)) currentMax list
            in
            findMaxId decodedTasks 0

        model = initialModel
    in
    ( { model 
        | tasks = decodedTasks
        , tags = decodedTags
        , nextId = maxId + 1 
      }
    , Cmd.none 
    )

main : Program Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , update = updateWithStorage
        , view = viewRoot
        , subscriptions = \_ -> Sub.none
        }