import { Elm } from './src/Main.elm';
import "./src/assets/styles/main.scss";

// --- INITIALISIERUNG ---

// Wir laden gespeicherte Daten direkt beim Start, um sie als Flags oder via Port zu senden
const savedConfig = localStorage.getItem('stopwatch_config');
const savedLaps = localStorage.getItem('stopwatch_laps');

const app = Elm.Main.init({
    node: document.getElementById('elm-app'),
    flags: {
        backendUrl: "" // Bleibt leer, da wir lokal arbeiten
    }
});

// Sobald Elm bereit ist, schicken wir die geladenen Daten in die App
if (savedConfig) {
    setTimeout(() => {
        sendToElm('configReceiver', JSON.parse(savedConfig));
    }, 100);
}

// --- HELPER: ELM PORTS ---

const subscribeSafe = (portName: string, callback: (data: any) => void) => {
    const port = (app.ports as any)[portName];
    if (port && port.subscribe) port.subscribe(callback);
};

const sendToElm = (portName: string, data: any) => {
    const port = (app.ports as any)[portName];
    if (port && port.send) port.send(data);
};

// --- CORE LOGIC: BROWSER INTERACTION ---

/**
 * Aktualisiert den Titel des Browser-Tabs.
 * Wird von Elm bei jedem "Tick" aufgerufen.
 */
subscribeSafe('setPageTitle', (title: string) => {
    document.title = title;
});

/**
 * Speichert die Konfiguration (Nightmode, Format, etc.)
 */
subscribeSafe('saveConfig', (config: any) => {
    localStorage.setItem('stopwatch_config', JSON.stringify(config));
    
    // Optional: Theme-Klasse am Body toggeln für globales CSS
    if (config.nightMode) {
        document.body.classList.add('dark-theme');
    } else {
        document.body.classList.remove('dark-theme');
    }
});

/**
 * Speichert die Rundenliste separat
 */
subscribeSafe('saveLaps', (laps: any) => {
    localStorage.setItem('stopwatch_laps', JSON.stringify(laps));
});

// --- FULLSCREEN API ---

subscribeSafe('triggerFullscreen', () => {
    const elem = document.documentElement;
    if (elem.requestFullscreen) {
        elem.requestFullscreen();
    }
});

subscribeSafe('exitFullscreen', () => {
    if (document.exitFullscreen) {
        document.exitFullscreen();
    }
});

// --- NAVIGATION ---

subscribeSafe('pushUrl', (url: string) => {
    history.pushState({}, '', url);
});

// --- VISIBILITY CHANGE ---
// Verhindert, dass der Browser den Timer im Hintergrund komplett einfriert
document.addEventListener('visibilitychange', () => {
    if (document.visibilityState === 'visible') {
        // Hier könnte man einen Sync-Trigger an Elm senden, falls nötig
    }
});