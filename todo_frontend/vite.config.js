import { defineConfig } from 'vite';
import elmPlugin from 'vite-plugin-elm';
import { viteSingleFile } from "vite-plugin-singlefile";
import tailwindcss from '@tailwindcss/postcss'; // Neu für v4
import autoprefixer from 'autoprefixer';
import cssnano from 'cssnano';

export default defineConfig({
  plugins: [
    elmPlugin(),
    // viteSingleFile() // Kann für finalen Build (eine HTML Datei) aktiviert werden
  ],

  css: {
    postcss: {
      plugins: [
        tailwindcss(), 
        autoprefixer(),
        cssnano({
          preset: 'default',
        }),
      ],
    },
  },

  server: {
    port: 3010,
    strictPort: true,
    // Wichtig für SPA-Routing, falls wir URLs wie /stoppuhr nutzen
    historyApiFallback: true, 
    // Proxy entfernt, da wir kein R-Backend mehr brauchen
  },

  base: '/', 

  build: {
    target: 'esnext',
    assetsInlineLimit: 10000, 
    cssCodeSplit: false,
    minify: 'terser',
    terserOptions: {
      compress: {
        drop_console: true, 
      },
    },
    sourcemap: false
  }
});