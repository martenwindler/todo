import { Elm } from './src/Main.elm';
import "./src/assets/styles/main.scss";

// 1. HELPER: Safe JSON Parsing
const safeParse = (key: string, fallback: any) => {
    try {
        const item = localStorage.getItem(key);
        return item ? JSON.parse(item) : fallback;
    } catch (e) {
        console.error(`Error parsing LocalStorage key "${key}":`, e);
        return fallback;
    }
};

// 2. PULL DATA WITH FALLBACKS
const savedTasks = safeParse('todo_tasks', []);
const savedTags = safeParse('todo_tags', []);
const savedInputs = safeParse('todo_inputs', { taskInput: "", tagInput: "" });

// 3. START THE ELM APP
const app = Elm.Main.init({
    node: document.getElementById('elm-app'),
    flags: {
        tasks: savedTasks,
        tags: savedTags,
        taskInput: savedInputs.taskInput || "",
        tagInput: savedInputs.tagInput || ""
    }
});

// 4. PORTS (The "Persistence" layer)

app.ports.saveTasks.subscribe((tasks: any) => {
    localStorage.setItem('todo_tasks', JSON.stringify(tasks));
});

app.ports.saveTags.subscribe((tags: any) => {
    localStorage.setItem('todo_tags', JSON.stringify(tags));
});

app.ports.saveInputs.subscribe((inputs: any) => {
    localStorage.setItem('todo_inputs', JSON.stringify(inputs));
});

// DEADLINE LOGIC
app.ports.requestDateTimestamp.subscribe((data: { id: number, dateStr: string }) => {
    // Validating the date string before converting
    const timestamp = data.dateStr ? new Date(data.dateStr).getTime() : null;
    
    // Send back to Elm immediately
    if (app.ports.receiveDateTimestamp) {
        app.ports.receiveDateTimestamp.send({ 
            id: data.id, 
            timestamp: timestamp 
        });
    }
});