
import { defineConfig, loadEnv } from 'vite';
import react from '@vitejs/plugin-react';
// import react from './node_modules/@vitejs/plugin-react/dist/index.mjs';
import tailwindcss from '@tailwindcss/vite';
import path from 'path';

export default defineConfig(({ mode }) => {
  // 1. Cargamos las variables desde la raíz (2 niveles arriba)
  // El tercer parámetro '' carga todas las variables, no solo las que empiezan con VITE_
  const envDir = path.resolve(__dirname, '../../');
  console.log({envDir})
  const env = loadEnv(mode, envDir, '');

  // 2. Mapeamos las variables para que Vite las reconozca
  // Creamos un objeto que contenga solo lo que necesitamos o transformamos nombres
  const processEnvValues = {
    'process.env': Object.entries(env).reduce((prev, [key, val]) => {
      return {
        ...prev,
        [key]: val,
      };
    }, {}),
  };

  return {
    plugins: [
      react(),
      tailwindcss(),
    ],
    resolve: {
      alias: {
        "@": path.resolve(__dirname, "./src"),
      },
    },
    define: {
      global: 'globalThis',
      ...processEnvValues, // Esto expone TODAS las variables del .env externo
    },

    server: {
      headers: {
        "Cross-Origin-Opener-Policy": "same-origin",
        "Cross-Origin-Embedder-Policy": "require-corp",
      },
    },
    
    optimizeDeps: {
      exclude: ['@ffmpeg/ffmpeg', '@ffmpeg/util'],
      include: ['mp4box'],
    },
    
    // Opcional: Si quieres que Vite específicamente use el prefijo VITE_ para seguridad
    envPrefix: ['VITE_', 'CANISTER_', 'DFX_'],
    envDir: '../../', 
  };
});
