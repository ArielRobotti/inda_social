// components/settings/forms/shared/DigitalPresenceFields.tsx
import React, { useState } from 'react';
import { Plus, XCircle, Share2, LinkIcon, Check, X } from 'lucide-react';
import { SOCIAL_CONFIG } from '../../../utils/socialConfigs';
import { MetadataPart, Value } from '@/declarations/backend/backend.did';

interface Props {
  extraData: MetadataPart[];
  updateMetadata: ({ key, value }: MetadataPart) => void;
  nameLabel?: string;
}

const DigitalPresenceFields = ({ extraData, updateMetadata, nameLabel = "Public Name" }: Props) => {
  const [showPicker, setShowPicker] = useState(false);
  const [activeNetwork, setActiveNetwork] = useState<string | null>(null);
  const [urlInput, setUrlInput] = useState("");

  // --- LÓGICA DE EXTRACCIÓN DEL ESTADO UNIFICADO ---
  const artisticName = extraData.find(m => m.key === "artisticName")?.value && 'Text' in extraData.find(m => m.key === "artisticName")!.value
    ? (extraData.find(m => m.key === "artisticName")!.value as { Text: string }).Text
    : "";

  const networks = (() => {
    const networksPart = extraData.find(m => m.key === "networks");
    if (networksPart?.value && 'Map' in networksPart.value) {
      return Object.fromEntries(
        networksPart.value.Map.map(([netCode, val]) => {
          // Forzamos la extracción del string de la variante Text
          const username = ('Text' in val) ? (val.Text as string) : ""; 
          return [netCode, username];
        })
      ) as Record<string, string>; // <--- Casteo preventivo
    }
    return {} as Record<string, string>;
  })();

  const hasNetworks = Object.keys(networks).length > 0;

  // --- LÓGICA DE ACTUALIZACIÓN ---
  const handleNameChange = (val: string) => {
    updateMetadata({ key: "artisticName", value: { Text: val } });
  };

  const handleAddNetwork = () => {
    if (!activeNetwork) return;

    const config = SOCIAL_CONFIG[activeNetwork as keyof typeof SOCIAL_CONFIG];
    const match = urlInput.match(config.pattern);

    if (match) {
      const username = match[1] || match[0];

      // 1. Obtenemos las redes actuales del registro "networks" (si existe)
      // Recordamos que 'networks' (variable local) ya es un Record<string, string> 
      // extraído en el cuerpo del componente.
      const updatedNetworksRecord = {
        ...networks,
        [activeNetwork]: username
      };

      // 2. Construimos el Array de Tuplas para el Value de tipo Map
      // Cada elemento debe ser [string, Value]
      const networkMapEntries: Array<[string, Value]> = Object.entries(updatedNetworksRecord).map(
      ([netCode, user]) => {
        const valueEntry: Value = { Text: user }; 
        return [netCode, valueEntry];
      }
    );

      // 3. Enviamos el MetadataPart unificado
      // Key: "networks", Value: { Map: [[string, Value]] }
      updateMetadata({
        key: "networks",
        value: { Map: networkMapEntries }
      });

      closeModal();
    } else {
      alert(`Invalid ${config.label} URL format`);
    }
  };

  // const removeNetwork = (type: string) => {
  //   const newNets = { ...networks };
  //   delete newNets[type];

  //   const networkMap = Object.entries(newNets).map(([k, v]) =>
  //     [k, { Text: v }] as [string, MetadataValue]
  //   );

  //   updateMetadata("networks", { Map: networkMap });
  // };

  const closeModal = () => {
    setActiveNetwork(null);
    setUrlInput("");
    setShowPicker(false);
  };

  return (
    <div className="relative space-y-4 pt-6 border-t border-white/5">
      <div className="flex items-center justify-between">
        <label className="text-[10px] text-zinc-500 font-bold uppercase tracking-widest">
          Digital Presence
        </label>
        {!showPicker && hasNetworks && (
          <span className="text-[9px] text-inda-blue/50 font-medium uppercase tracking-tighter">
            Connect more platforms
          </span>
        )}
      </div>

      <input
        placeholder={`${nameLabel} (e.g. Fiodor 3D / Inda Corp)`}
        className="w-full bg-zinc-900 border border-white/10 p-3 rounded-xl text-sm outline-none focus:border-inda-blue text-white transition-all"
        value={artisticName}
        onChange={(e) => handleNameChange(e.target.value)}
      />

      <div className="flex flex-wrap gap-2 items-center">
        {Object.entries(networks).map(([type, user]) => (
          <div key={type} className="flex items-center gap-2 bg-inda-blue/10 border border-inda-blue/30 px-3 py-1.5 rounded-full animate-in zoom-in-95">
            <div className="w-4 h-4 flex items-center justify-center text-inda-blue">
              {SOCIAL_CONFIG[type as keyof typeof SOCIAL_CONFIG].icon}
            </div>
            <span className="text-[10px] font-bold text-white">@{user}</span>
            <button
              type="button"
              // onClick={() => removeNetwork(type)}
              className="text-white/40 hover:text-red-400 transition-colors"
            >
              <XCircle size={14} />
            </button>
          </div>
        ))}

        <button
          type="button"
          onClick={() => setShowPicker(!showPicker)}
          className={`flex items-center gap-2 transition-all duration-300 ${!hasNetworks ? "w-full justify-center py-4 bg-white/5 border border-dashed border-white/10 rounded-2xl hover:bg-inda-blue/5" : "p-2 bg-white/5 border border-white/10 rounded-full"}`}
        >
          <Plus size={18} className="text-inda-blue" />
          {!hasNetworks && <span className="text-xs text-zinc-400 font-medium">Link your social media profiles</span>}
        </button>
      </div>

      {/* Selector de Redes */}
      {showPicker && (
        <div className="grid grid-cols-2 sm:grid-cols-3 gap-2 p-3 bg-black/40 border border-white/5 rounded-2xl animate-in zoom-in-95">
          {Object.entries(SOCIAL_CONFIG).map(([id, cfg]) => (
            <button
              key={id}
              disabled={!!networks[id]}
              onClick={() => setActiveNetwork(id)}
              className="flex items-center gap-2 p-2.5 rounded-xl hover:bg-inda-blue/10 text-[10px] text-zinc-300 disabled:opacity-20 transition-all group"
            >
              <div className="w-5 h-5 text-zinc-400 group-hover:text-inda-blue transition-colors">
                {cfg.icon}
              </div>
              {cfg.label}
            </button>
          ))}
        </div>
      )}

      {/* Modal de URL (Ajustado para ser relative/absolute correctamente) */}
      {activeNetwork && (
        <div className="absolute inset-x-0 top-20 z-30 flex items-center justify-center p-1 bg-zinc-950/90 backdrop-blur-sm rounded-2xl animate-in fade-in zoom-in-95">
          {/* ... Contenido del modal igual al anterior ... */}
          <div className="w-full bg-zinc-900 border border-white/10 p-4 rounded-2xl shadow-2xl space-y-4">
            <div className="flex items-center justify-between">
              <div className="flex items-center gap-3">
                <div className="w-6 h-6 text-inda-blue">
                  {SOCIAL_CONFIG[activeNetwork as keyof typeof SOCIAL_CONFIG].icon}
                </div>
                <div className="flex flex-col">
                  <p className="text-[10px] text-zinc-500 font-bold uppercase leading-none mb-1">Social Platform</p>
                  <p className="text-[12px] font-black text-white uppercase leading-none">Link {SOCIAL_CONFIG[activeNetwork as keyof typeof SOCIAL_CONFIG].label}</p>
                </div>
              </div>
              <button onClick={closeModal} className="text-zinc-500 hover:text-white"><X size={16} /></button>
            </div>
            <div className="relative">
              <LinkIcon size={14} className="absolute left-3 top-1/2 -translate-y-1/2 text-inda-blue/50" />
              <input
                autoFocus
                placeholder={`https://${activeNetwork}.com/your-user`}
                className="w-full bg-black/40 border border-white/10 pl-9 pr-4 py-3 rounded-xl text-xs text-white outline-none focus:border-inda-blue"
                value={urlInput}
                onChange={(e) => setUrlInput(e.target.value)}
                onKeyDown={(e) => e.key === 'Enter' && handleAddNetwork()}
              />
            </div>
            <div className="flex gap-2">
              <button onClick={closeModal} className="flex-1 py-2 text-[10px] font-bold text-zinc-500 uppercase">Cancel</button>
              <button onClick={handleAddNetwork} disabled={!urlInput} className="flex-1 bg-inda-blue py-2 rounded-xl text-[10px] font-bold text-white uppercase disabled:opacity-50 flex items-center justify-center gap-2">
                <Check size={14} /> Confirm
              </button>
            </div>
          </div>
        </div>
      )}
    </div>
  );
};

export default DigitalPresenceFields;