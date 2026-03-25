import { Elm } from './src/Main.elm';
import "./src/assets/styles/main.scss";

// 1. PULL DATA FROM BROWSER STORAGE
// We use 'todo_tasks' and 'todo_tags' to match the names in our ports
const savedTasks = localStorage.getItem('todo_tasks');
const savedTags = localStorage.getItem('todo_tags');

// 2. START THE ELM APP
const app = Elm.Main.init({
    node: document.getElementById('elm-app'),
    flags: {
        // We parse the string back into JSON. 
        // If it's empty, we pass null so Elm handles the default.
        tasks: savedTasks ? JSON.parse(savedTasks) : null,
        tags: savedTags ? JSON.parse(savedTags) : null
    }
});

// 3. LISTEN FOR SAVES (The "Write" part of CRUD)
// This triggers every time you add/edit/delete in Elm
app.ports.saveTasks.subscribe((tasks: any) => {
    console.log("Saving tasks to localStorage...", tasks);
    localStorage.setItem('todo_tasks', JSON.stringify(tasks));
});

app.ports.saveTags.subscribe((tags: any) => {
    localStorage.setItem('todo_tags', JSON.stringify(tags));
});