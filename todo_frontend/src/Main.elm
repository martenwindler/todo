module Main exposing (..)

import Browser
import Html exposing (Html, a, button, div, h1, input, label, li, span, text, ul)
import Html.Attributes as Attr exposing (class, checked, href, placeholder, style, value, autofocus)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit, onBlur)
import Json.Decode as Decode
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
    { tasks = [ Task { id = 1, content = "Add tags above and click them here!", completed = False, tags = [], subtasks = [] } ]
    , input = ""
    , tagInput = ""
    , activeFilter = All
    , activeTag = "all"
    , tags = [] 
    , nextId = 2
    , editingId = Nothing
    , editInput = ""
    }

-- 2. UPDATE

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

update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateInput str -> { model | input = str }
        UpdateTagInput str -> { model | tagInput = str }
        
        StartEdit id currentContent -> 
            { model | editingId = Just id, editInput = currentContent }
            
        UpdateEditInput str -> { model | editInput = str }
        
        SaveEdit ->
            case model.editingId of
                Just id ->
                    let
                        rename (Task t) = Task { t | content = model.editInput }
                    in
                    { model | tasks = updateHelper id rename model.tasks, editingId = Nothing, editInput = "" }
                Nothing -> model

        CancelEdit -> { model | editingId = Nothing, editInput = "" }

        AddTask ->
            if String.isEmpty model.input then model else
                let newTask = Task { id = model.nextId, content = model.input, completed = False, tags = [], subtasks = [] }
                in { model | tasks = newTask :: model.tasks, input = "", nextId = model.nextId + 1 }

        AddSubtask pId ->
            let newS = Task { id = model.nextId, content = "New Subtask", completed = False, tags = [], subtasks = [] }
                add (Task t) = Task { t | subtasks = t.subtasks ++ [newS] }
            in { model | tasks = updateHelper pId add model.tasks, nextId = model.nextId + 1 }

        ToggleTask id ->
            let toggle (Task t) = Task { t | completed = not t.completed }
            in { model | tasks = updateHelper id toggle model.tasks }

        DeleteTask id -> { model | tasks = deleteHelper id model.tasks }
        MoveRight id -> { model | tasks = indentRight id model.tasks }
        MoveLeft id -> 
            let (updated, extracted) = indentLeft id model.tasks
            in { model | tasks = updated ++ extracted }

        CreateGlobalTag ->
            if String.isEmpty model.tagInput || List.any (\t -> t.name == model.tagInput) model.tags then model else
                let newTag = { name = model.tagInput, colour = "#5c3df5" }
                in { model | tags = model.tags ++ [newTag], tagInput = "" }

        DeleteGlobalTag tagName ->
            let 
                -- Recursive function to strip the deleted tag from all tasks in the tree
                stripTag (Task t) = 
                    Task { t | tags = List.filter (\n -> n /= tagName) t.tags
                           , subtasks = List.map stripTag t.subtasks 
                         }
            in 
            { model | tags = List.filter (\t -> t.name /= tagName) model.tags
            , tasks = List.map stripTag model.tasks 
            , activeTag = if model.activeTag == tagName then "all" else model.activeTag
            }

        ToggleTagOnTask taskId tagName ->
            let toggle (Task t) = 
                    let newTags = if List.member tagName t.tags then List.filter (\n -> n /= tagName) t.tags else tagName :: t.tags 
                    in Task { t | tags = newTags }
            in { model | tasks = updateHelper taskId toggle model.tasks }

        SetFilter f -> { model | activeFilter = f }
        SetTag t -> { model | activeTag = t }
        Reset -> initialModel

-- Helpers
updateHelper id fn list = List.map (\(Task t) -> if t.id == id then fn (Task t) else Task { t | subtasks = updateHelper id fn t.subtasks }) list
deleteHelper id list = list |> List.filter (\(Task t) -> t.id /= id) |> List.map (\(Task t) -> Task { t | subtasks = deleteHelper id t.subtasks })
indentRight targetId list = case list of
    [] -> []
    [only] -> let (Task t) = only in [ Task { t | subtasks = indentRight targetId t.subtasks } ]
    (Task f) :: (Task s) :: r -> if s.id == targetId then [ Task { f | subtasks = f.subtasks ++ [Task s] } ] ++ r else Task { f | subtasks = indentRight targetId f.subtasks } :: indentRight targetId (Task s :: r)
indentLeft targetId list = let folder (Task t) (accL, accE) = if t.id == targetId then (accL, accE ++ [Task t]) else let (newS, ext) = indentLeft targetId t.subtasks in (accL ++ [Task { t | subtasks = newS }], accE ++ ext) in List.foldl folder ([], []) list

-- 3. VIEW

view : Model -> Html Msg
view model =
    div [ class "wrap" ]
        [ h1 [] [ text "Todo" ]
        , div [ class "app" ]
            [ viewForm model.input
            , viewTagManager model
            , viewTaskList model
            , viewControls model
            ]
        ]

viewTagManager : Model -> Html Msg
viewTagManager model = 
    div [ class "task-tags", style "padding" "1rem", style "border-bottom" "1px solid #eee" ] 
        [ div [ style "display" "flex", style "gap" "10px", style "margin-bottom" "10px" ]
            [ input [ placeholder "Create a tag...", value model.tagInput, onInput UpdateTagInput, style "flex-grow" "1" ] []
            , button [ onClick CreateGlobalTag, style "background" "#5c3df5", style "color" "white", style "border" "none", style "padding" "0 15px", style "border-radius" "4px" ] [ text "Add Tag" ] 
            ]
        , div [ style "display" "flex", style "flex-wrap" "wrap", style "gap" "5px" ]
            (List.map (\tag -> 
                span [ style "background" "#eee", style "padding" "2px 8px", style "border-radius" "12px", style "font-size" "12px", style "display" "flex", style "align-items" "center" ] 
                    [ text tag.name
                    , button [ onClick (DeleteGlobalTag tag.name), style "border" "none", style "background" "none", style "color" "#ff4444", style "cursor" "pointer", style "margin-left" "5px", style "font-weight" "bold" ] [ text "×" ] 
                    ]
            ) model.tags)
        ]

viewTaskList : Model -> Html Msg
viewTaskList model =
    let
        -- Only show tasks that match the active tag filter
        filterByTag tasks =
            if model.activeTag == "all" then tasks
            else 
                tasks 
                |> List.filter (\(Task t) -> List.member model.activeTag t.tags)
    in
    ul [ class "task-list" ] (List.map (viewTaskItem model) (filterByTag model.tasks))

viewTaskItem : Model -> Task -> Html Msg
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
                span [ style "flex-grow" "1", style "cursor" "text", class (if t.completed then "task-item-checked" else ""), onClick (StartEdit t.id t.content) ] 
                    [ text t.content, span [ class "task-strike" ] [] ]
            
            -- Tag Toggles on Task
            , div [ style "display" "flex", style "gap" "3px" ] 
                (List.map (\tag -> 
                    button 
                        [ onClick (ToggleTagOnTask t.id tag.name)
                        , style "font-size" "10px"
                        , style "border" "none"
                        , style "border-radius" "3px"
                        , style "cursor" "pointer"
                        , style "padding" "2px 5px"
                        , style "background" (if List.member tag.name t.tags then "#5c3df5" else "#f0f0f0")
                        , style "color" (if List.member tag.name t.tags then "white" else "#666")
                        ] [ text tag.name ]
                ) model.tags)

            , button [ class "add-sub-btn", onClick (AddSubtask t.id) ] [ text "+" ]
            , button [ class "task-item-remove", onClick (DeleteTask t.id) ]
                [ svg [ SvgAttr.viewBox "0 0 40 40" ] [ path [ SvgAttr.d "M15 15 L25 25 M25 15 L15 25" ] [] ] ]
            ]
        , if List.isEmpty t.subtasks then text "" else 
            div [ style "margin-left" "35px", style "border-left" "1px solid #eee" ] 
                (List.map (viewTaskItem model) t.subtasks)
        ]

onEnter msg = Html.Events.on "keydown" (Decode.field "key" Decode.string |> Decode.andThen (\k -> if k == "Enter" then Decode.succeed msg else Decode.fail ""))
viewForm i = Html.form [ class "task-form", onSubmit AddTask ] [ input [ placeholder "What to do?", value i, onInput UpdateInput ] [], button [ Attr.type_ "submit" ] [ text "Add Task" ] ]
viewControls model = 
    div [ class "task-controls" ] 
        [ span [ style "font-size" "12px", style "color" "#888" ] [ text "Filter: " ]
        , button [ onClick (SetTag "all"), class (if model.activeTag == "all" then "btn-active" else "") ] [ text "All" ]
        , div [ style "display" "inline-block" ]
            (List.map (\tag -> button [ onClick (SetTag tag.name), class (if model.activeTag == tag.name then "btn-active" else "") ] [ text tag.name ]) model.tags)
        ]

main = Browser.sandbox { init = initialModel, update = update, view = view }