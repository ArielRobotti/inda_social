
import "./index.css"; 
import React from 'react';
import "@nfid/identitykit/react/styles.css";
import { createRoot } from 'react-dom/client';
import { IdentityKitProvider } from "@nfid/identitykit/react";
import App from './App';
import { identityKitConfig } from "./identity/config";
import { SessionProvider } from "./context/SessionContext";

createRoot(document.getElementById('root')!).render(
  <React.StrictMode>
    <IdentityKitProvider {...identityKitConfig}>
      <SessionProvider>
          <App />
      </SessionProvider>
    </IdentityKitProvider>
  </React.StrictMode>
);