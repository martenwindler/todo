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
const app = (Elm as any).Main.init({
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

// DEADLINE LOGIC - FIXED TIMEZONE OFFSET
app.ports.requestDateTimestamp.subscribe((data: { id: number, dateStr: string }) => {
    if (!data.dateStr) {
        app.ports.receiveDateTimestamp.send({ id: data.id, timestamp: null });
        return;
    }

    // Input format: "2026-03-25T15:00"
    const [datePart, timePart] = data.dateStr.split('T');
    const [year, month, day] = datePart.split('-').map(Number);
    const [hours, minutes] = timePart.split(':').map(Number);

    // This creates a Date object using LOCAL system time parts
    // Month is 0-indexed in JS (January is 0), so we subtract 1
    const localDate = new Date(year, month - 1, day, hours, minutes);
    
    if (app.ports.receiveDateTimestamp) {
        app.ports.receiveDateTimestamp.send({ 
            id: data.id, 
            timestamp: localDate.getTime() 
        });
    }
});