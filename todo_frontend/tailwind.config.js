/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    "./index.html",
    "./src/**/*.elm", // Wichtig: Scannt deinen Elm-Code
    "./src/**/*.ts",  // Scannt deine TypeScript-Dateien
  ],
  theme: {
    // Hier ist aktuell nichts definiert, 
    // Tailwind nutzt die Default-Themes (Standardfarben/Abstände).
    extend: {},
  },
  plugins: [],
}