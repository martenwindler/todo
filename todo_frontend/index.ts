import { Elm } from './src/Main.elm';
import "./src/assets/styles/main.scss";

// 1. PULL DATA FROM BROWSER STORAGE
const savedTasks = localStorage.getItem('todo_tasks');
const savedTags = localStorage.getItem('todo_tags');
const savedInputs = localStorage.getItem('todo_inputs');

// Parse the inputs or provide defaults so Elm doesn't crash
const parsedInputs = savedInputs ? JSON.parse(savedInputs) : { taskInput: "", tagInput: "" };

// 2. START THE ELM APP
const app = Elm.Main.init({
    node: document.getElementById('elm-app'),
    flags: {
        tasks: savedTasks ? JSON.parse(savedTasks) : null,
        tags: savedTags ? JSON.parse(savedTags) : null,
        // New: Pass the temporary input strings as flags
        taskInput: parsedInputs.taskInput,
        tagInput: parsedInputs.tagInput
    }
});

// 3. LISTEN FOR SAVES (The "Write" part of CRUD)

// Saves the main task tree
app.ports.saveTasks.subscribe((tasks: any) => {
    console.log("Saving tasks to localStorage...", tasks);
    localStorage.setItem('todo_tasks', JSON.stringify(tasks));
});

// Saves the global tags list
app.ports.saveTags.subscribe((tags: any) => {
    localStorage.setItem('todo_tags', JSON.stringify(tags));
});

// NEW: Saves the current text in the input fields (Autosave as you type)
app.ports.saveInputs.subscribe((inputs: any) => {
    localStorage.setItem('todo_inputs', JSON.stringify(inputs));
});