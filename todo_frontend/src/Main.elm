module Main exposing (..)

import Browser
import Html exposing (Html, a, button, div, h1, input, label, li, span, text, ul, select, option)
import Html.Attributes as Attr exposing (class, checked, href, placeholder, style, value, autofocus, id, selected)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit, onBlur)
import Html.Keyed as Keyed
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Ports
import Svg exposing (svg, path)
import Svg.Attributes as SvgAttr
import Time
import Task

-- 1. MODEL

type TaskCategory = Today | Upcoming | Anytime | Someday | Logbook

type Task
    = Task
        { id : Int
        , content : String
        , completed : Bool
        , tags : List String
        , category : TaskCategory
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
    , activeCategory : TaskCategory
    , tags : List Tag
    , nextId : Int
    , editingId : Maybe Int 
    , editInput : String
    , lastSaved : Maybe Time.Posix
    , zone : Time.Zone
    }

initialModel : Model
initialModel =
    { tasks = []
    , input = ""
    , tagInput = ""
    , activeFilter = All
    , activeTag = "all"
    , activeCategory = Today
    , tags = [] 
    , nextId = 1
    , editingId = Nothing
    , editInput = ""
    , lastSaved = Nothing
    , zone = Time.utc
    }

-- 2. JSON ENCODERS

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
        ]

encodeTag : Tag -> Encode.Value
encodeTag tag =
    Encode.object [ ("name", Encode.string tag.name), ("colour", Encode.string tag.colour) ]

encodeInputs : Model -> Encode.Value
encodeInputs model =
    Encode.object
        [ ("taskInput", Encode.string model.input)
        , ("tagInput", Encode.string model.tagInput)
        ]

-- 3. JSON DECODERS

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
    Decode.map6 (\id_ content completed tags category subtasks -> Task { id = id_, content = content, completed = completed, tags = tags, category = category, subtasks = subtasks })
        (Decode.field "id" Decode.int) (Decode.field "content" Decode.string) (Decode.field "completed" Decode.bool)
        (Decode.field "tags" (Decode.list Decode.string)) (Decode.field "category" categoryDecoder)
        (Decode.lazy (\_ -> Decode.field "subtasks" (Decode.list taskDecoder)))

tagDecoder : Decoder Tag
tagDecoder = Decode.map2 Tag (Decode.field "name" Decode.string) (Decode.field "colour" Decode.string)

-- 4. UPDATE

type Msg
    = UpdateInput String | UpdateTagInput String | AddTask | AddSubtask Int | DeleteTask Int | ToggleTask Int
    | MoveTask Int TaskCategory | MoveRight Int | MoveLeft Int | SetFilter Filter | SetTag String | SetCategory TaskCategory
    | Reset | CreateGlobalTag | DeleteGlobalTag String | ToggleTagOnTask Int String | StartEdit Int String | UpdateEditInput String
    | SaveEdit | CancelEdit | ClearCompleted | ClearAll | RecordSaveTime Time.Posix | AdjustTimeZone Time.Zone

updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        newModel = update msg model
        isChange = case msg of
            UpdateInput _ -> True
            UpdateTagInput _ -> True
            AddTask -> True
            AddSubtask _ -> True
            DeleteTask _ -> True
            ToggleTask _ -> True
            MoveTask _ _ -> True
            SaveEdit -> True
            ClearCompleted -> True
            ClearAll -> True
            SetCategory _ -> True
            MoveRight _ -> True
            MoveLeft _ -> True
            ToggleTagOnTask _ _ -> True
            CreateGlobalTag -> True
            DeleteGlobalTag _ -> True
            _ -> False
        saveCmds = if isChange then Cmd.batch [ Ports.saveTasks (Encode.list encodeTask newModel.tasks), Ports.saveTags (Encode.list encodeTag newModel.tags), Ports.saveInputs (encodeInputs newModel), Task.perform RecordSaveTime Time.now ] else Cmd.none
    in ( newModel, saveCmds )

update : Msg -> Model -> Model
update msg model =
    case msg of
        RecordSaveTime t -> { model | lastSaved = Just t }
        AdjustTimeZone z -> { model | zone = z }
        UpdateInput str -> { model | input = str }
        UpdateTagInput str -> { model | tagInput = str }
        SetCategory cat -> { model | activeCategory = cat }
        AddTask ->
            if String.isEmpty model.input then model 
            else { model | tasks = Task { id = model.nextId, content = model.input, completed = False, tags = [], category = model.activeCategory, subtasks = [] } :: model.tasks, input = "", nextId = model.nextId + 1 }
        
        MoveTask id newCat -> 
            { model | tasks = updateCategoryRecursive id newCat model.tasks }

        StartEdit tid c -> { model | editingId = Just tid, editInput = c }
        UpdateEditInput str -> { model | editInput = str }
        SaveEdit -> case model.editingId of 
                Just tid -> { model | tasks = updateHelper tid (\(Task t) -> Task { t | content = model.editInput }) model.tasks, editingId = Nothing }
                Nothing -> model
        CancelEdit -> { model | editingId = Nothing }
        ToggleTask tid -> { model | tasks = toggleAndCheckParent tid model.tasks }
        AddSubtask pId -> { model | tasks = updateHelper pId (\(Task t) -> Task { t | subtasks = t.subtasks ++ [Task { id = model.nextId, content = "New Subtask", completed = False, tags = [], category = t.category, subtasks = [] }], completed = False }) model.tasks, nextId = model.nextId + 1 }
        DeleteTask tid -> { model | tasks = deleteHelper tid model.tasks }
        MoveRight tid -> { model | tasks = indentRight tid model.tasks }
        MoveLeft tid -> let (upd, ext) = indentLeft tid model.tasks in { model | tasks = upd ++ ext }
        CreateGlobalTag -> if String.isEmpty model.tagInput || List.any (\t -> t.name == model.tagInput) model.tags then model else { model | tags = model.tags ++ [{ name = model.tagInput, colour = "#5c3df5" }], tagInput = "" }
        DeleteGlobalTag n -> { model | tags = List.filter (\t -> t.name /= n) model.tags, tasks = List.map (stripTag n) model.tasks }
        ToggleTagOnTask tid n -> { model | tasks = updateHelper tid (\(Task t) -> Task { t | tags = if List.member n t.tags then List.filter ((/=) n) t.tags else n :: t.tags }) model.tasks }
        SetFilter f -> { model | activeFilter = f }
        SetTag t -> { model | activeTag = t }
        Reset -> initialModel
        ClearCompleted -> { model | tasks = filterComp model.tasks }
        ClearAll -> { model | tasks = [], tags = [] }

-- 5. HELPERS

updateCategoryRecursive : Int -> TaskCategory -> List Task -> List Task
updateCategoryRecursive targetId newCat list =
    List.map (\(Task t) ->
        if t.id == targetId then applyCategoryToTree newCat (Task t)
        else Task { t | subtasks = updateCategoryRecursive targetId newCat t.subtasks }
    ) list

applyCategoryToTree : TaskCategory -> Task -> Task
applyCategoryToTree newCat (Task t) =
    Task { t | category = newCat, subtasks = List.map (applyCategoryToTree newCat) t.subtasks }

updateHelper id fn list = List.map (\(Task t) -> if t.id == id then fn (Task t) else Task { t | subtasks = updateHelper id fn t.subtasks }) list
deleteHelper id list = list |> List.filter (\(Task t) -> t.id /= id) |> List.map (\(Task t) -> Task { t | subtasks = deleteHelper id t.subtasks })
stripTag n (Task t) = Task { t | tags = List.filter ((/=) n) t.tags, subtasks = List.map (stripTag n) t.subtasks }
filterComp tasks = tasks |> List.filter (\(Task t) -> not t.completed) |> List.map (\(Task t) -> Task { t | subtasks = filterComp t.subtasks })
countAll list = List.foldl (\(Task t) acc -> 1 + acc + countAll t.subtasks) 0 list
countDone list = List.foldl (\(Task t) acc -> (if t.completed then 1 else 0) + acc + countDone t.subtasks) 0 list

indentRight targetId list = 
    case list of 
        [] -> []
        [only] -> let (Task t) = only in [ Task { t | subtasks = indentRight targetId t.subtasks } ]
        (Task f) :: (Task s) :: r -> if s.id == targetId then [ Task { f | subtasks = f.subtasks ++ [Task s] } ] ++ r else Task { f | subtasks = indentRight targetId f.subtasks } :: indentRight targetId (Task s :: r)

indentLeft targetId list = 
    let folder (Task t) (accL, accE) = if t.id == targetId then (accL, accE ++ [Task t]) else let (newS, ext) = indentLeft targetId t.subtasks in (accL ++ [Task { t | subtasks = newS }], accE ++ ext) 
    in List.foldl folder ([], []) list

toggleAndCheckParent targetId list = List.map (\(Task t) -> 
    let updatedSub = if t.id == targetId then t.subtasks else toggleAndCheckParent targetId t.subtasks 
    in Task { t | subtasks = updatedSub, completed = if t.id == targetId then not t.completed else if not (List.isEmpty updatedSub) then List.all (\(Task sub) -> sub.completed) updatedSub else t.completed }) list

-- 6. VIEW

formatTime zone time =
    let hour = String.fromInt (Time.toHour zone time)
        minute = String.padLeft 2 '0' (String.fromInt (Time.toMinute zone time))
        second = String.padLeft 2 '0' (String.fromInt (Time.toSecond zone time))
    in hour ++ ":" ++ minute ++ ":" ++ second

viewLastSaved model = case model.lastSaved of
    Just t -> span [ style "font-size" "12px", style "color" "#888", style "margin-left" "10px" ] [ text ("Last saved at " ++ formatTime model.zone t) ]
    Nothing -> text ""

viewProgress subtasks =
    let total = countAll subtasks
        done = countDone subtasks
        percentage = if total == 0 then 0 else (toFloat done / toFloat total) * 100
        deg = percentage * 3.6
        rS = if percentage >= 50 then [ style "background" "inherit", style "transform" ("rotate(" ++ String.fromFloat (deg - 180) ++ "deg)") ] else [ style "background" "#ccc", style "transform" ("rotate(" ++ String.fromFloat deg ++ "deg)") ]
    in div [ class "progress-holder", style "width" "34px", style "height" "34px" ] [ div [ class "semi-circle" ] [], div [ class "left-block" ] [], div (class "right-block" :: rS) [], div [ class "mask", style "width" "76%", style "height" "76%", style "top" "12%", style "left" "12%" ] [ span [ style "font-size" "9px", style "top" "calc(50% - 6px)" ] [ text (String.fromInt (round percentage) ++ "%") ] ] ]

viewTaskItem : Model -> Task -> ( String, Html Msg )
viewTaskItem model (Task t) =
    let 
        isEditing = model.editingId == Just t.id
        toCat str = case str of 
            "Today" -> Today
            "Upcoming" -> Upcoming
            "Anytime" -> Anytime
            "Someday" -> Someday
            "Logbook" -> Logbook
            _ -> Anytime
        
        catStr = categoryToString t.category
        taskKey = catStr ++ "-" ++ (String.fromInt t.id)
        viewOption val labelTxt = option [ Attr.value val, selected (catStr == val) ] [ text labelTxt ]
    in 
    ( taskKey
    , li [] [ div [ class "task-row-container", style "display" "flex", style "align-items" "center", style "gap" "8px" ] 
            [ button [ onClick (MoveLeft t.id), class "nav-btn" ] [ text "‹" ], button [ onClick (MoveRight t.id), class "nav-btn" ] [ text "›" ]
            , select [ onInput (\s -> MoveTask t.id (toCat s)), value catStr, id ("sel-" ++ taskKey), style "font-size" "10px" ] 
                [ viewOption "Today" "T", viewOption "Upcoming" "U", viewOption "Anytime" "A", viewOption "Someday" "S", viewOption "Logbook" "L" ]
            , input [ Attr.type_ "checkbox", checked t.completed, onCheck (\_ -> ToggleTask t.id) ] []
            , if isEditing then input [ value model.editInput, onInput UpdateEditInput, onBlur SaveEdit, onEnter SaveEdit, autofocus True, style "flex-grow" "1", class "edit-input" ] []
              else span [ style "flex-grow" "1", class (if t.completed then "task-item-checked" else ""), onClick (StartEdit t.id t.content) ] [ text t.content, span [ class "task-strike" ] [] ]
            , if not (List.isEmpty t.subtasks) then viewProgress t.subtasks else text ""
            , div [ style "display" "flex", style "gap" "3px" ] (List.map (\tag -> button [ onClick (ToggleTagOnTask t.id tag.name), style "font-size" "10px", style "background" (if List.member tag.name t.tags then "#5c3df5" else "#f0f0f0"), style "color" (if List.member tag.name t.tags then "white" else "#666"), style "border" "none", style "border-radius" "3px" ] [ text tag.name ]) model.tags)
            , button [ class "add-sub-btn", onClick (AddSubtask t.id) ] [ text "+" ], button [ class "task-item-remove", onClick (DeleteTask t.id) ] [ svg [ SvgAttr.viewBox "0 0 40 40" ] [ path [ SvgAttr.d "M15 15 L25 25 M25 15 L15 25" ] [] ] ]
            ], if List.isEmpty t.subtasks then text "" else div [ style "margin-left" "35px", style "border-left" "1px solid #eee" ] [ Keyed.ul [ class "task-list" ] (List.map (viewTaskItem model) t.subtasks) ] 
        ]
    )

onEnter msg = Html.Events.on "keydown" (Decode.field "key" Decode.string |> Decode.andThen (\k -> if k == "Enter" then Decode.succeed msg else Decode.fail ""))

viewRoot model = 
    let 
        filteredByCat = 
            if model.activeCategory == Logbook then model.tasks 
            else List.filter (\(Task t) -> t.category == model.activeCategory) model.tasks

        filtered = filteredByCat |> List.filter (\(Task t) -> if model.activeTag == "all" then True else List.member model.activeTag t.tags)
            
        getBadgeCount c =
            if c == Logbook then List.length model.tasks
            else List.length (List.filter (\(Task t) -> t.category == c) model.tasks)
    in 
    div [ class "wrap" ] 
        [ h1 [] [ text "Todo", viewLastSaved model ]
        , div [ class "app" ] 
            [ div [ class "category-bar", style "margin-bottom" "20px" ] 
                (List.map (\c -> button [ onClick (SetCategory c), class (if model.activeCategory == c then "btn-active" else "") ] [ text (categoryToString c ++ " (" ++ String.fromInt (getBadgeCount c) ++ ")") ]) [Today, Upcoming, Anytime, Someday, Logbook])
            , Html.form [ class "task-form", onSubmit AddTask ] [ input [ placeholder "New Task...", value model.input, onInput UpdateInput ] [], button [ Attr.type_ "submit" ] [ text "Add" ] ]
            , div [ class "task-tags" ] 
                [ div [ style "display" "flex", style "gap" "10px" ] 
                    [ input [ placeholder "Tag name", value model.tagInput, onInput UpdateTagInput, style "flex-grow" "1" ] [], button [ onClick CreateGlobalTag ] [ text "Add Tag" ] ]
                , div [ style "display" "flex", style "gap" "5px", style "margin-top" "5px" ] 
                    (List.map (\tag -> span [ class "tag-label" ] [ text tag.name, button [ onClick (DeleteGlobalTag tag.name), style "border" "none", style "background" "none", style "color" "red" ] [ text "×" ] ]) model.tags) 
                ]
            , Keyed.ul [ class "task-list" ] (List.map (viewTaskItem model) filtered)
            , div [ class "task-controls" ] 
                [ button [ onClick (SetTag "all"), class (if model.activeTag == "all" then "btn-active" else "") ] [ text "All Tags" ]
                , div [ style "display" "inline-block", style "margin-left" "10px" ] (List.map (\t -> button [ onClick (SetTag t.name), class (if model.activeTag == t.name then "btn-active" else "") ] [ text t.name ]) model.tags)
                , div [ style "float" "right", style "display" "flex", style "gap" "10px" ] 
                    [ button [ onClick ClearCompleted ] [ text "Clear Completed" ], button [ onClick ClearAll, style "background" "#ff4444", style "color" "white", style "border" "none", style "padding" "0 10px", style "border-radius" "4px" ] [ text "Clear All" ] ] 
                ] 
            ] 
        ]

init flags =
    let dTasks = Decode.decodeValue (Decode.field "tasks" (Decode.list taskDecoder)) flags |> Result.withDefault []
        dTags = Decode.decodeValue (Decode.field "tags" (Decode.list tagDecoder)) flags |> Result.withDefault []
        sInp = Decode.decodeValue (Decode.field "taskInput" Decode.string) flags |> Result.withDefault ""
        sTInp = Decode.decodeValue (Decode.field "tagInput" Decode.string) flags |> Result.withDefault ""
        mId = let findM list cur = List.foldl (\(Task t) m -> findM t.subtasks (max m t.id)) cur list in findM dTasks 0
    in ( { initialModel | tasks = dTasks, tags = dTags, input = sInp, tagInput = sTInp, nextId = mId + 1 }, Task.perform AdjustTimeZone Time.here )

main = Browser.element { init = init, update = updateWithStorage, view = viewRoot, subscriptions = \_ -> Sub.none }