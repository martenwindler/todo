module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, input, li, span, text, select, option)
import Html.Attributes as Attr exposing (class, checked, placeholder, style, value, autofocus, id, selected, type_, property)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit, onBlur, on)
import Html.Keyed as Keyed
import Json.Decode as Decode
import Json.Encode as Encode
import Ports
import Svg exposing (svg, path)
import Svg.Attributes as SvgAttr
import Time
import Task
import Decoders as D exposing (Task(..), TaskCategory(..))

-- 1. MODEL
type alias Model =
    { tasks : List Task
    , input : String
    , tagInput : String
    , activeTag : String
    , activeCategory : TaskCategory
    , tags : List D.Tag
    , nextId : Int
    , editingId : Maybe Int 
    , editInput : String
    , lastSaved : Maybe Time.Posix
    , currentTime : Maybe Time.Posix
    , zone : Time.Zone
    }

initialModel : Model
initialModel =
    { tasks = []
    , input = ""
    , tagInput = ""
    , activeTag = "all"
    , activeCategory = Today
    , tags = [] 
    , nextId = 1
    , editingId = Nothing
    , editInput = ""
    , lastSaved = Nothing
    , currentTime = Nothing
    , zone = Time.utc
    }

-- 2. UPDATE
updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        newModel = update msg model
        isChange = case msg of
            AddTask -> True
            DeleteTask _ -> True
            ToggleTask _ -> True
            MoveTask _ _ -> True
            FinishDeadline _ -> True
            SaveEdit -> True
            ClearCompleted -> True
            ClearAll -> True
            ToggleTagOnTask _ _ -> True
            CreateGlobalTag -> True
            DeleteGlobalTag _ -> True
            _ -> False
        
        saveCmds = 
            if isChange then 
                Cmd.batch 
                    [ Ports.saveTasks (Encode.list D.encodeTask newModel.tasks)
                    , Ports.saveTags (Encode.list D.encodeTag newModel.tags)
                    , Ports.saveInputs (D.encodeInputs { input = newModel.input, tagInput = newModel.tagInput })
                    , Task.perform RecordSaveTime Time.now 
                    ] 
            else Cmd.none
            
        portCmd = case msg of
            SetDeadline id_ dateStr -> 
                if String.isEmpty dateStr || String.length dateStr >= 16 then
                    Ports.requestDateTimestamp { id = id_, dateStr = dateStr }
                else 
                    Cmd.none
            _ -> Cmd.none
    in ( newModel, Cmd.batch [ saveCmds, portCmd ] )

type Msg
    = UpdateInput String | UpdateTagInput String | AddTask | AddSubtask Int | DeleteTask Int | ToggleTask Int
    | MoveTask Int TaskCategory | MoveRight Int | MoveLeft Int | SetTag String | SetCategory TaskCategory
    | Reset | CreateGlobalTag | DeleteGlobalTag String | ToggleTagOnTask Int String | StartEdit Int String | UpdateEditInput String
    | SaveEdit | CancelEdit | ClearCompleted | ClearAll | RecordSaveTime Time.Posix | AdjustTimeZone Time.Zone
    | SetDeadline Int String | FinishDeadline { id : Int, timestamp : Maybe Int } | Tick Time.Posix

update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick t -> { model | currentTime = Just t }
        RecordSaveTime t -> { model | lastSaved = Just t }
        AdjustTimeZone z -> { model | zone = z }
        UpdateInput str -> { model | input = str }
        UpdateTagInput str -> { model | tagInput = str }
        SetCategory cat -> { model | activeCategory = cat }
        AddTask ->
            if String.isEmpty model.input then model 
            else { model | tasks = Task { id = model.nextId, content = model.input, completed = False, tags = [], category = model.activeCategory, subtasks = [], deadline = Nothing } :: model.tasks, input = "", nextId = model.nextId + 1 }
        
        SetDeadline _ _ -> model 
        
        FinishDeadline data ->
            { model | tasks = updateHelper data.id (\(Task t) -> Task { t | deadline = data.timestamp }) model.tasks }

        MoveTask id newCat -> { model | tasks = updateCategoryRecursive id newCat model.tasks }
        StartEdit tid c -> { model | editingId = Just tid, editInput = c }
        UpdateEditInput str -> { model | editInput = str }
        SaveEdit -> case model.editingId of 
                Just tid -> { model | tasks = updateHelper tid (\(Task t) -> Task { t | content = model.editInput }) model.tasks, editingId = Nothing }
                Nothing -> model
        CancelEdit -> { model | editingId = Nothing }
        ToggleTask tid -> { model | tasks = toggleAndCheckParent tid model.tasks }
        AddSubtask pId -> { model | tasks = updateHelper pId (\(Task t) -> Task { t | subtasks = t.subtasks ++ [Task { id = model.nextId, content = "New Subtask", completed = False, tags = [], category = t.category, subtasks = [], deadline = Nothing }], completed = False }) model.tasks, nextId = model.nextId + 1 }
        DeleteTask tid -> { model | tasks = deleteHelper tid model.tasks }
        MoveRight tid -> { model | tasks = indentRight tid model.tasks }
        MoveLeft tid -> let (upd, ext) = indentLeft tid model.tasks in { model | tasks = upd ++ ext }
        CreateGlobalTag -> if String.isEmpty model.tagInput || List.any (\t -> t.name == model.tagInput) model.tags then model else { model | tasks = model.tasks, tags = model.tags ++ [{ name = model.tagInput, colour = "#5c3df5" }], tagInput = "" }
        DeleteGlobalTag n -> { model | tags = List.filter (\t -> t.name /= n) model.tags, tasks = List.map (stripTag n) model.tasks }
        ToggleTagOnTask tid n -> { model | tasks = updateHelper tid (\(Task t) -> Task { t | tags = if List.member n t.tags then List.filter ((/=) n) t.tags else n :: t.tags }) model.tasks }
        SetTag t -> { model | activeTag = t }
        Reset -> initialModel
        ClearCompleted -> { model | tasks = filterComp model.tasks }
        ClearAll -> { model | tasks = [], tags = [] }

-- 3. HELPERS
toCategory : String -> TaskCategory
toCategory s =
    case s of
        "Today" -> Today
        "Upcoming" -> Upcoming
        "Anytime" -> Anytime
        "Someday" -> Someday
        "Logbook" -> Logbook
        _ -> Anytime

updateCategoryRecursive id newCat list =
    List.map (\(Task t) -> if t.id == id then applyCategoryToTree newCat (Task t) else Task { t | subtasks = updateCategoryRecursive id newCat t.subtasks }) list

applyCategoryToTree newCat (Task t) =
    Task { t | category = newCat, subtasks = List.map (applyCategoryToTree newCat) t.subtasks }

updateHelper id fn list = List.map (\(Task t) -> if t.id == id then fn (Task t) else Task { t | subtasks = updateHelper id fn t.subtasks }) list
deleteHelper id list = list |> List.filter (\(Task t) -> t.id /= id) |> List.map (\(Task t) -> Task { t | subtasks = deleteHelper id t.subtasks })
stripTag n (Task t) = Task { t | tags = List.filter ((/=) n) t.tags, subtasks = List.map (stripTag n) t.subtasks }
filterComp tasks = tasks |> List.filter (\(Task t) -> not t.completed) |> List.map (\(Task t) -> Task { t | subtasks = filterComp t.subtasks })

indentRight targetId list = case list of 
    [] -> []
    (Task f) :: (Task s) :: r -> if s.id == targetId then [ Task { f | subtasks = f.subtasks ++ [Task s] } ] ++ r else Task { f | subtasks = indentRight targetId f.subtasks } :: indentRight targetId (Task s :: r)
    any :: r -> any :: indentRight targetId r

indentLeft targetId list = let folder (Task t) (accL, accE) = if t.id == targetId then (accL, accE ++ [Task t]) else let (newS, ext) = indentLeft targetId t.subtasks in (accL ++ [Task { t | subtasks = newS }], accE ++ ext) in List.foldl folder ([], []) list

toggleAndCheckParent targetId list = List.map (\(Task t) -> let updatedSub = if t.id == targetId then t.subtasks else toggleAndCheckParent targetId t.subtasks in Task { t | subtasks = updatedSub, completed = if t.id == targetId then not t.completed else if not (List.isEmpty updatedSub) then List.all (\(Task sub) -> sub.completed) updatedSub else t.completed }) list

-- 4. VIEW
viewDeadline : Time.Zone -> Maybe Time.Posix -> Maybe Int -> Html Msg
viewDeadline zone maybeNow deadline =
    case deadline of
        Just target ->
            let
                now = Maybe.withDefault (Time.millisToPosix 0) maybeNow
                getStartOfDay millis =
                    let p = Time.millisToPosix millis
                    in (Time.toYear zone p * 366) + (D.monthToInt (Time.toMonth zone p) * 31) + Time.toDay zone p
                
                todayStart = getStartOfDay (Time.posixToMillis now)
                targetDay = getStartOfDay target
                diff = targetDay - todayStart
                
                targetPosix = Time.millisToPosix target
                timeTxt = " @ " ++ String.fromInt (Time.toHour zone targetPosix) ++ ":" ++ String.padLeft 2 '0' (String.fromInt (Time.toMinute zone targetPosix))
                
                (txt, color) = 
                    if diff < 0 then ("Overdue", "#ff4444")
                    else if diff == 0 then ("Today" ++ timeTxt, "#5c3df5")
                    else if diff == 1 then ("Tomorrow" ++ timeTxt, "#3d5cf5")
                    else ("In " ++ String.fromInt diff ++ " days", "#888")
            in 
            span [ style "font-size" "10px", style "color" color, style "font-weight" "bold", style "margin" "0 10px" ] [ text txt ]
        _ -> text ""

-- Replace your current viewTaskItem with this version:

viewTaskItem : Model -> Task -> ( String, Html Msg )
viewTaskItem model (Task t) =
    let 
        isEditing = model.editingId == Just t.id
        catStr = D.categoryToString t.category
        taskKey = catStr ++ "-" ++ (String.fromInt t.id)
        
        -- Generate the string from the model
        pickerValue = D.millisToIsoDateTime model.zone t.deadline
        
        -- THE FIX: A key that is unique to this task AND its current value.
        -- This forces Elm to "pin" this specific input to this task.
        stableKey = "task-row-" ++ String.fromInt t.id ++ "-" ++ pickerValue
    in 
    ( stableKey
    , li [ id stableKey ] [ div [ class "task-row-container", style "display" "flex", style "align-items" "center", style "gap" "8px" ] 
            [ button [ onClick (MoveLeft t.id), class "nav-btn" ] [ text "‹" ], button [ onClick (MoveRight t.id), class "nav-btn" ] [ text "›" ]
            , select [ onInput (\s -> MoveTask t.id (toCategory s)), value catStr, id ("sel-" ++ taskKey), style "font-size" "10px" ] 
                [ option [ Attr.value "Today", selected (catStr == "Today") ] [ text "T" ]
                , option [ Attr.value "Upcoming", selected (catStr == "Upcoming") ] [ text "U" ]
                , option [ Attr.value "Anytime", selected (catStr == "Anytime") ] [ text "A" ]
                , option [ Attr.value "Someday", selected (catStr == "Someday") ] [ text "S" ]
                , option [ Attr.value "Logbook", selected (catStr == "Logbook") ] [ text "L" ] 
                ]
            , input [ Attr.type_ "checkbox", checked t.completed, onCheck (\_ -> ToggleTask t.id) ] []
            , if isEditing then input [ value model.editInput, onInput UpdateEditInput, onBlur SaveEdit, onEnter SaveEdit, autofocus True, style "flex-grow" "1", class "edit-input" ] []
              else span [ style "flex-grow" "1", class (if t.completed then "task-item-checked" else ""), onClick (StartEdit t.id t.content) ] [ text t.content, span [ class "task-strike" ] [] ]
            
            , viewDeadline model.zone model.currentTime t.deadline
            
            , div [ class "date-picker-wrap" ] 
                [ input 
                    [ type_ "datetime-local"
                    -- WE FORCE BOTH PROPERTY AND ATTRIBUTE
                    , property "value" (Encode.string pickerValue)
                    , Attr.value pickerValue
                    , on "change" (Decode.map (SetDeadline t.id) Decode.string)
                    , class "stable-date-picker"
                    ] 
                    []
                ]
            , div [ style "display" "flex", style "gap" "3px" ] (List.map (\tag -> button [ onClick (ToggleTagOnTask t.id tag.name), style "font-size" "10px", style "background" (if List.member tag.name t.tags then "#5c3df5" else "#f0f0f0"), style "color" (if List.member tag.name t.tags then "white" else "#666"), style "border" "none", style "border-radius" "3px" ] [ text tag.name ]) model.tags)
            , button [ class "add-sub-btn", onClick (AddSubtask t.id) ] [ text "+" ], button [ class "task-item-remove", onClick (DeleteTask t.id) ] [ svg [ SvgAttr.viewBox "0 0 40 40" ] [ path [ SvgAttr.d "M15 15 L25 25 M25 15 L15 25" ] [] ] ]
            ], if List.isEmpty t.subtasks then text "" else div [ style "margin-left" "35px", style "border-left" "1px solid #eee" ] [ Keyed.ul [ class "task-list" ] (List.map (viewTaskItem model) t.subtasks) ] 
        ]
    )
    
viewRoot : Model -> Html Msg
viewRoot model =
    let
        filteredByCat =
            if model.activeCategory == Logbook then model.tasks
            else List.filter (\(Task t) -> t.category == model.activeCategory) model.tasks

        filtered =
            filteredByCat
                |> List.filter
                    (\(Task t) ->
                        if model.activeTag == "all" then True
                        else List.member model.activeTag t.tags
                    )

        getBadgeCount c =
            if c == Logbook then List.length model.tasks
            else List.length (List.filter (\(Task t) -> t.category == c) model.tasks)

        saveTimeLabel =
            case model.lastSaved of
                Just t ->
                    span [ style "font-size" "12px", style "color" "#888", style "margin-left" "10px" ]
                        [ text ("Saved: " ++ String.fromInt (Time.toHour model.zone t) ++ ":" ++ String.padLeft 2 '0' (String.fromInt (Time.toMinute model.zone t))) ]
                Nothing -> text ""
    in
    div [ class "wrap" ]
        [ h1 [] [ text "Todo", saveTimeLabel ]
        , div [ class "app" ]
            [ div [ class "category-bar", style "margin-bottom" "20px" ]
                (List.map
                    (\c ->
                        button
                            [ onClick (SetCategory c)
                            , class (if model.activeCategory == c then "btn-active" else "")
                            ]
                            [ text (D.categoryToString c ++ " (" ++ String.fromInt (getBadgeCount c) ++ ")") ]
                    )
                    [ Today, Upcoming, Anytime, Someday, Logbook ]
                )
            , Html.form [ class "task-form", onSubmit AddTask ]
                [ input [ placeholder "New Task...", value model.input, onInput UpdateInput ] []
                , button [ Attr.type_ "submit" ] [ text "Add" ]
                ]
            , div [ class "task-tags" ]
                [ div [ style "display" "flex", style "gap" "10px" ]
                    [ input [ placeholder "Tag name", value model.tagInput, onInput UpdateTagInput, style "flex-grow" "1" ] []
                    , button [ onClick CreateGlobalTag ] [ text "Add Tag" ]
                    ]
                , div [ style "display" "flex", style "gap" "5px", style "margin-top" "5px" ]
                    (List.map
                        (\tag ->
                            span [ class "tag-label" ]
                                [ text tag.name
                                , button [ onClick (DeleteGlobalTag tag.name), style "border" "none", style "background" "none", style "color" "red" ] [ text "×" ]
                                ]
                        )
                        model.tags
                    )
                ]
            , Keyed.ul [ class "task-list" ] (List.map (viewTaskItem model) filtered)
            , div [ class "task-controls" ]
                [ button [ onClick (SetTag "all"), class (if model.activeTag == "all" then "btn-active" else "") ] [ text "All Tags" ]
                , div [ style "display" "inline-block", style "margin-left" "10px" ]
                    (List.map
                        (\t ->
                            button
                                [ onClick (SetTag t.name)
                                , class (if model.activeTag == t.name then "btn-active" else "")
                                ]
                                [ text t.name ]
                        )
                        model.tags
                    )
                , div [ style "float" "right", style "display" "flex", style "gap" "10px" ]
                    [ button [ onClick ClearCompleted ] [ text "Clear Completed" ]
                    , button [ onClick ClearAll, style "background" "#ff4444", style "color" "white", style "border" "none", style "padding" "0 10px", style "border-radius" "4px" ] [ text "Clear All" ]
                    ]
                ]
            ]
        ]

onEnter msg = Html.Events.on "keydown" (Decode.field "key" Decode.string |> Decode.andThen (\k -> if k == "Enter" then Decode.succeed msg else Decode.fail ""))

init flags =
    let dTasks = Decode.decodeValue (Decode.field "tasks" (Decode.list D.taskDecoder)) flags |> Result.withDefault []
        dTags = Decode.decodeValue (Decode.field "tags" (Decode.list D.tagDecoder)) flags |> Result.withDefault []
        sInp = Decode.decodeValue (Decode.field "taskInput" Decode.string) flags |> Result.withDefault ""
        sTInp = Decode.decodeValue (Decode.field "tagInput" Decode.string) flags |> Result.withDefault ""
        findMaxId list currentMax = List.foldl (\(Task t) acc -> findMaxId t.subtasks (max acc t.id)) currentMax list
        mId = findMaxId dTasks 0
    in ( { initialModel | tasks = dTasks, tags = dTags, input = sInp, tagInput = sTInp, nextId = mId + 1 }, Cmd.batch [ Task.perform Tick Time.now, Task.perform AdjustTimeZone Time.here ] )

main = Browser.element { init = init, update = updateWithStorage, view = viewRoot, subscriptions = \_ -> Ports.receiveDateTimestamp FinishDeadline }