// components/settings/forms/shared/DigitalPresenceFields.tsx
import React, { useState } from 'react';
import { Plus, XCircle } from 'lucide-react';
import { SOCIAL_CONFIG } from '../../../utils/socialConfigs';

interface Props {
  data: { name: string; networks: Record<string, string> };
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  onChange: (newData: any) => void;
  nameLabel?: string;
}

const DigitalPresenceFields = ({ data, onChange, nameLabel = "Public Name" }: Props) => {
  const [showPicker, setShowPicker] = useState(false);

  const addNetwork = (type: string, url: string) => {
    const config = SOCIAL_CONFIG[type as keyof typeof SOCIAL_CONFIG];
    const match = url.match(config.pattern);
    if (match) {
      const username = match[1] || match[0];
      onChange({ ...data, networks: { ...data.networks, [type]: username } });
    }
  };

  const removeNetwork = (type: string) => {
    const newNets = { ...data.networks };
    delete newNets[type];
    onChange({ ...data, networks: newNets });
  };

  return (
    <div className="space-y-4 pt-4 border-t border-white/5">
      <label className="text-[10px] text-zinc-500 font-bold uppercase tracking-widest">Digital Presence</label>
      
      <input
        placeholder={`${nameLabel} (e.g. Fiodor 3D / Inda Corp)`}
        className="w-full bg-zinc-900 border border-white/10 p-3 rounded-xl text-sm outline-none focus:border-inda-blue text-white"
        value={data.name}
        onChange={(e) => onChange({ ...data, name: e.target.value })}
      />

      <div className="flex flex-wrap gap-2">
        {Object.entries(data.networks).map(([type, user]) => (
          <div key={type} className="flex items-center gap-2 bg-inda-blue/10 border border-inda-blue/30 px-3 py-1.5 rounded-full">
            <span className="text-xs">{SOCIAL_CONFIG[type as keyof typeof SOCIAL_CONFIG].icon}</span>
            <span className="text-[10px] font-bold text-white">@{user}</span>
            <button onClick={() => removeNetwork(type)} className="text-white/40 hover:text-red-400">
              <XCircle size={14} />
            </button>
          </div>
        ))}
        <button 
          type="button"
          onClick={() => setShowPicker(!showPicker)}
          className="p-2 bg-white/5 border border-white/10 rounded-full hover:border-inda-blue/50"
        >
          <Plus size={18} className="text-inda-blue" />
        </button>
      </div>

      {showPicker && (
        <div className="grid grid-cols-2 sm:grid-cols-3 gap-2 p-3 bg-black/40 border border-white/5 rounded-2xl">
          {Object.entries(SOCIAL_CONFIG).map(([id, cfg]) => (
            <button
              key={id}
              disabled={!!data.networks[id]}
              onClick={() => {
                const url = prompt(`Link to ${cfg.label}:`);
                if (url) addNetwork(id, url);
                setShowPicker(false);
              }}
              className="flex items-center gap-2 p-2 rounded-lg hover:bg-white/5 text-[10px] text-zinc-300 disabled:opacity-20"
            >
              {cfg.icon} {cfg.label}
            </button>
          ))}
        </div>
      )}
    </div>
  );
};

export default DigitalPresenceFields;