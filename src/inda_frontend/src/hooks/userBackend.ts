import { useIdentity } from "@nfid/identitykit/react";
import { HttpAgent, Actor } from "@dfinity/agent";
import { idlFactory } from "../declarations/backend"; 
import type { _SERVICE } from "../declarations/backend/backend.did";
import { useCallback } from "react"; 

export const useBackend = () => {
  const identity = useIdentity();

  // Envolvemos todo en useCallback para que la función sea estable
  const getBackendActor = useCallback(async () => {
    const isMainnet = import.meta.env.DFX_NETWORK === "ic";
    const host = isMainnet ? "https://icp-api.io" : "http://127.0.0.1:4943";

    const agent = HttpAgent.createSync({
        host,
        identity,
        shouldFetchRootKey: !isMainnet,
    });

    if (!isMainnet) {
      await agent.fetchRootKey().catch((err) => {
        console.warn("No se pudo obtener la RootKey.", err);
      });
    }

    return Actor.createActor<_SERVICE>(idlFactory, {
      agent,
      canisterId: import.meta.env.CANISTER_ID_BACKEND,
    });
  }, [identity]); 

  return { getBackendActor };
};