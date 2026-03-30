import React, { createContext, useContext, useEffect, useState, useCallback } from "react";
import { useIdentity, useAuth } from "@nfid/identitykit/react";
import { useBackend } from "../hooks/useBackend";
import type { _SERVICE } from "../declarations/backend/backend.did";
import type { User, Creator, Brand, Partnership, UserDataInit, EditableData } from "../declarations/backend/backend.did"
import { Principal } from "@dfinity/principal";
import { ActorSubclass } from "@dfinity/agent";

// Tipos basados en tu backend
interface SessionContextType {
  backend: ActorSubclass<_SERVICE>;
  user: User | null;
  role: Creator | Brand | Partnership | null;
  isAdmin: boolean;
  loading: boolean;
  needsRegistration: boolean;
  principalID: string;
  refreshSession: () => Promise<User>;
  logout: () => void
  signUp: (data: UserDataInit) => Promise<User | null>;
  updateProfile: (data: EditableData) => Promise<User | null>;
  // Aquí puedes agregar más cosas a futuro, como:
  // notifications: any[]; 
}

const SessionContext = createContext<SessionContextType | undefined>(undefined);

export const SessionProvider: React.FC<{ children: React.ReactNode }> = ({ children }) => {
  const identity = useIdentity();
  const { disconnect } = useAuth();
  const [backend, setBackend] = useState(null) ;
  
  // const [isConnected, setIsconnected] = useState(false)
  const [principalID, setPrincipalID] = useState("")
  const [user, setUser] = useState<User | null>(null);
  const [role, setRole] = useState<Creator | Brand | Partnership | null>(null)
  const [isAdmin, setIsAdmin] = useState(false)
  const [loading, setLoading] = useState(false);
  const [needsRegistration, setNeedsRegistration] = useState(false);

  const { getBackendActor } = useBackend()
  

  const logout = useCallback(async () => {
    setLoading(true);
    try {
      // disconnect() de useAuth limpia la identidad en el storage y el estado del kit
      await disconnect(); 
      // Limpiamos nuestro estado local del CRM
      setUser(null);
      setNeedsRegistration(false);
      setPrincipalID("")
      setLoading(false)

    } catch (e) {
      console.error("Error al cerrar sesión:", e);
    } finally {
      setLoading(false);
    }
  }, [disconnect]);

  const refreshSession = useCallback(async () => {
    const isAnonymous = !identity || identity.getPrincipal().isAnonymous();
    // console.log(identity.getPrincipal().toText())

    if (isAnonymous) {
      setUser(null);
      setNeedsRegistration(false);
      setPrincipalID("")
      setLoading(false);
      return;
    } else {
      setPrincipalID(identity.getPrincipal().toText())
    }
    
    try {
      setLoading(true);
      // const backend = await getBackendActor();
      const isUserAdmin = await backend.isAdmin([])
      setIsAdmin( isUserAdmin)
      const loginResult = await backend.login();
      // console.log({loginResult})

      if ("Ok" in loginResult) {
        const userData = loginResult.Ok.user;
        const userRole = loginResult.Ok.creator[0] || loginResult.Ok.brand[0] || loginResult.Ok.partner[0] || null;    
        
        setUser(userData);
        setRole(userRole)
        setNeedsRegistration(false);
        return userData;
      } else {
        setUser(null);
        setNeedsRegistration(true);
        return null;
      };
    } catch (error) {
      console.error("❌ Error fatal en refreshSession:", error);
    } finally {
      setLoading(false);
    }
  }, [identity, backend]);

  const signUp = async(data: UserDataInit) => {
    try {
      setLoading(true);
      const backend = await getBackendActor();
      const signUpResult = await backend.signUp(data);
      if("Ok" in signUpResult) {
        setUser(signUpResult.Ok)
        return signUpResult.Ok
      } else {
        return null
      }

    } catch(e) {
      console.log(e)
      return null
    }
  };

  const updateProfile = async (data: EditableData) => {
    try {
      setLoading(true);
      // const backend = await getBackendActor();
      if(!backend) return
      const signUpResult = await backend.editProfile(data);
      console.log(signUpResult)
      if("Ok" in signUpResult) {
        setUser(signUpResult.Ok)
        return signUpResult.Ok
      } else {
        return null
      }

    } catch(e) {
      console.log(e)
      return null
    }
  }

  useEffect(() => {
    refreshSession();
  }, [refreshSession]);

  useEffect(() => {
    const fetcBackend = async () => {
      setBackend(await getBackendActor())
    }
    fetcBackend()
  },[getBackendActor])

  return (
    <SessionContext.Provider value={{ user, backend, role, isAdmin, loading, needsRegistration, refreshSession, logout, signUp, updateProfile, principalID }}>
      {children}
    </SessionContext.Provider>
  );
};

// Hook personalizado para usar la sesión fácilmente
// eslint-disable-next-line react-refresh/only-export-components
export const useSession = () => {
  const context = useContext(SessionContext);
  if (!context) throw new Error("useSession debe usarse dentro de SessionProvider");
  return context;
};