import React, { useState } from 'react';
import { Plus, XCircle } from 'lucide-react';
import Button from '../../components/Button';
import DigitalPresenceFields from './shared/DigitalPresenceFields';
import { COUNTRY_IDENTITY_CONFIG } from '@/utils/countryIdentityConfig';
import { CreatorDataInit, MetadataPart } from '@/declarations/backend/backend.did';

interface Props {
  onSubmit: (payload: CreatorDataInit) => void;
  isSaving: boolean;
}

const CreatorForm = ({ onSubmit, isSaving }: Props) => {
  // Estado Unificado del Formulario de Creador
  const [formData, setFormData] = useState({
    portfolio: '',
    webSite: '',
    govIdType: 'passport' as 'passport' | 'rfc' | 'ine' | 'dni',
    govIdValue: '',
  });

  // Estado para DigitalPresenceFields
  const [presence, setPresence] = useState({
    name: '',
    networks: {} as Record<string, string>,
  });

  const [selectedCountry, setSelectedCountry] = useState<keyof typeof COUNTRY_IDENTITY_CONFIG>('AR');
  const [docType, setDocType] = useState(COUNTRY_IDENTITY_CONFIG.AR.documents[0].id);
  const [docValue, setDocValue] = useState('');

  // Al cambiar el país, reseteamos el tipo de documento al primero disponible de ese país
  const handleCountryChange = (countryCode: string) => {
    const code = countryCode as keyof typeof COUNTRY_IDENTITY_CONFIG;
    setSelectedCountry(code);
    setDocType(COUNTRY_IDENTITY_CONFIG[code].documents[0].id);
    setDocValue('');
  };

  const handleValueChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const currentDoc = COUNTRY_IDENTITY_CONFIG[selectedCountry].documents.find(d => d.id === docType);
    let val = e.target.value;

    // 1. Si es numérico, eliminamos todo lo que no sea número
    if (currentDoc?.isNumeric) {
      val = val.replace(/\D/g, '');
    }

    // 2. Limitamos por cantidad de dígitos/caracteres
    if (currentDoc?.maxDigits && val.length > currentDoc.maxDigits) {
      val = val.slice(0, currentDoc.maxDigits);
    }

    setDocValue(val);
  };

  const handleInternalSubmit = () => {
    const extendedData: MetadataPart[] = [];
    if (presence.name) {
      extendedData.push({ key: "artisticName", value: { Text: presence.name } });
    }
    if (Object.keys(presence.networks).length > 0) {
      extendedData.push({
        key: "networks",
        value: { Map: Object.entries(presence.networks).map(([k, v]) => [k, { Text: v }]) }
      });
    }

    const payload: CreatorDataInit = {
      portfolio: formData.portfolio ? [formData.portfolio] : [],
      webSite: formData.webSite ? [formData.webSite] : [],
      governmentID: [{
        docType: `${docType}_${selectedCountry}`,
        value: docValue
      }],
      extendedData: extendedData,
    };

    onSubmit(payload);
  };

  const [extraData, setExtraData] = useState({
  birthDate: '',
  gender: 'not_specified',
  category: '3d_artist',
  country: 'AR',
  pitch: ''
});

// 2. Renderizado Compacto
return (
  <div className="max-w-3xl mx-auto bg-zinc-900/40 border border-white/5 rounded-[2.5rem] p-8 space-y-8 animate-in fade-in duration-500">
    
    <header className="border-b border-white/5 pb-4">
      <h3 className="text-xl font-bold text-white tracking-tight">Creator Application</h3>
      <p className="text-[10px] text-zinc-500 uppercase tracking-widest mt-1">Expansion Profile Details</p>
    </header>

    {/* SECCIÓN 1: DATOS PERSONALES & CATEGORÍA */}
    <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
      <div className="space-y-1">
        <label className="text-[10px] text-zinc-500 font-bold uppercase ml-1">Birth Date</label>
        <input 
          type="date" 
          className="w-full bg-black/40 border border-white/10 p-3 rounded-xl text-xs text-white outline-none focus:border-inda-blue color-scheme-dark"
          onChange={(e) => setExtraData({...extraData, birthDate: e.target.value})}
        />
      </div>

      <div className="space-y-1">
        <label className="text-[10px] text-zinc-500 font-bold uppercase ml-1">Gender</label>
        <select 
          className="w-full bg-black/40 border border-white/10 p-3 rounded-xl text-xs text-white outline-none focus:border-inda-blue"
          onChange={(e) => setExtraData({...extraData, gender: e.target.value})}
        >
          <option value="not_specified">Prefer not to say</option>
          <option value="male">Male</option>
          <option value="female">Female</option>
          <option value="non_binary">Non-binary</option>
        </select>
      </div>

      <div className="space-y-1">
        <label className="text-[10px] text-zinc-500 font-bold uppercase ml-1">Primary Focus</label>
        <select 
          className="w-full bg-black/40 border border-white/10 p-3 rounded-xl text-xs text-white outline-none focus:border-inda-blue"
          onChange={(e) => setExtraData({...extraData, category: e.target.value})}
        >
          <option value="3d_artist">3D Artist</option>
          <option value="fashion">Fashion Designer</option>
          <option value="content_creator">Content Creator</option>
          <option value="developer">Web3 Developer</option>
        </select>
      </div>
    </div>

    {/* SECCIÓN 2: PAÍS E IDENTIDAD (BLOQUE UNIFICADO Y COMPACTO) */}
    <div className="bg-white/5 p-5 rounded-3xl space-y-4">
      <div className="flex flex-col md:flex-row gap-4">
        <div className="flex-1 space-y-1">
          <label className="text-[10px] text-zinc-400 font-bold uppercase ml-1">Residence Country</label>
          <select
            className="w-full bg-zinc-900 border border-white/5 p-3 rounded-xl text-xs text-white outline-none"
            value={selectedCountry}
            onChange={(e) => handleCountryChange(e.target.value)}
          >
            {Object.entries(COUNTRY_IDENTITY_CONFIG).map(([code, config]) => (
              <option key={code} value={code}>{config.flag} {config.name}</option>
            ))}
          </select>
        </div>

        <div className="flex-1 space-y-1">
          <label className="text-[10px] text-zinc-400 font-bold uppercase ml-1">ID Type</label>
          <select
            className="w-full bg-zinc-900 border border-white/5 p-3 rounded-xl text-xs text-white outline-none"
            value={docType}
            onChange={(e) => setDocType(e.target.value)}
          >
            {COUNTRY_IDENTITY_CONFIG[selectedCountry].documents.map((doc) => (
              <option key={doc.id} value={doc.id}>{doc.label}</option>
            ))}
          </select>
        </div>

        <div className="flex-[2] space-y-1">
          <label className="text-[10px] text-zinc-400 font-bold uppercase ml-1">ID Number</label>
          <div className="relative">
            <input
              placeholder={COUNTRY_IDENTITY_CONFIG[selectedCountry].documents.find(d => d.id === docType)?.placeholder}
              className="w-full bg-zinc-900 border border-white/5 p-3 rounded-xl text-sm font-mono text-inda-blue outline-none"
              value={docValue}
              onChange={handleValueChange}
            />
            <span className="absolute right-3 top-3 text-[9px] text-zinc-600 font-bold uppercase">Verified</span>
          </div>
        </div>
      </div>
    </div>

    {/* SECCIÓN 3: PORTFOLIO & WEB */}
    <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
      <input
        placeholder="Portfolio URL (Behance, ArtStation, etc.)"
        className="bg-black/20 border border-white/10 p-4 rounded-2xl text-xs text-white outline-none focus:border-inda-blue"
        onChange={(e) => setFormData({ ...formData, portfolio: e.target.value })}
      />
      <input
        placeholder="Personal Website / Linktree"
        className="bg-black/20 border border-white/10 p-4 rounded-2xl text-xs text-white outline-none focus:border-inda-blue"
        onChange={(e) => setFormData({ ...formData, webSite: e.target.value })}
      />
    </div>

    {/* SECCIÓN 4: PRESENCIA DIGITAL (REDES) */}
    <DigitalPresenceFields data={presence} onChange={setPresence} nameLabel="Artistic / Brand Name" />

    {/* SECCIÓN 5: EL "PITCH" FINAL */}
    <div className="space-y-1">
      <label className="text-[10px] text-zinc-500 font-bold uppercase ml-1">Why do you want to be a Creator?</label>
      <textarea 
        rows={2}
        placeholder="Briefly describe your work and goals..."
        className="w-full bg-black/20 border border-white/10 p-4 rounded-2xl text-xs text-white outline-none focus:border-inda-blue resize-none"
        onChange={(e) => setExtraData({...extraData, pitch: e.target.value})}
      />
    </div>

    <Button
      onClick={handleInternalSubmit}
      className="w-full bg-inda-blue! text-white! py-5 rounded-[2rem] font-black uppercase tracking-[0.2em] text-[11px]"
      disabled={isSaving || !docValue || !presence.name}
    >
      {isSaving ? "Synchronizing with Blockchain..." : "Send Application"}
    </Button>
  </div>
);

  // return (
  //   <div className="bg-inda-blue/5 border border-inda-blue/20 rounded-[2rem] p-6 space-y-6 animate-in fade-in slide-in-from-top-4 duration-500">
  //     <h3 className="text-sm font-bold text-white mb-2 uppercase tracking-tight">Creator Application Details</h3>

  //     {/* Inputs de Portfolio y Website */}
  //     <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
  //       <div className="space-y-1">
  //         <label className="text-[10px] text-zinc-500 font-bold ml-1 uppercase">Professional Portfolio</label>
  //         <input
  //           placeholder="https://artstation.com/..."
  //           className="w-full bg-zinc-900 border border-white/10 p-3 rounded-xl text-sm text-white outline-none focus:border-inda-blue"
  //           value={formData.portfolio}
  //           onChange={(e) => setFormData({ ...formData, portfolio: e.target.value })}
  //         />
  //       </div>
  //       <div className="space-y-1">
  //         <label className="text-[10px] text-zinc-500 font-bold ml-1 uppercase">Official Website</label>
  //         <input
  //           placeholder="https://yoursite.com"
  //           className="w-full bg-zinc-900 border border-white/10 p-3 rounded-xl text-sm text-white outline-none focus:border-inda-blue"
  //           value={formData.webSite}
  //           onChange={(e) => setFormData({ ...formData, webSite: e.target.value })}
  //         />
  //       </div>
  //     </div>

  //     {/* Identificación Gubernamental */}
  //     <div className="max-w-2xl mx-auto space-y-6">
  //       <div className="bg-zinc-900/40 border border-white/5 p-6 rounded-[2.5rem] space-y-6">

  //         {/* FILA 1: PAÍS Y TIPO (MÁS ANGOSTOS) */}
  //         <div className="grid grid-cols-12 gap-3">
  //           <div className="col-span-5 space-y-1">
  //             <label className="text-[10px] text-zinc-500 font-bold ml-2 uppercase tracking-tighter">Country</label>
  //             <select
  //               className="w-full bg-black/40 border border-white/10 p-3 rounded-2xl text-xs text-white outline-none focus:border-inda-blue transition-colors cursor-pointer"
  //               value={selectedCountry}
  //               onChange={(e) => handleCountryChange(e.target.value)}
  //             >
  //               {Object.entries(COUNTRY_IDENTITY_CONFIG).map(([code, config]) => (
  //                 <option key={code} value={code}>{config.flag} {config.name}</option>
  //               ))}
  //             </select>
  //           </div>

  //           <div className="col-span-7 space-y-1">
  //             <label className="text-[10px] text-zinc-500 font-bold ml-2 uppercase tracking-tighter">Document Type</label>
  //             <select
  //               className="w-full bg-black/40 border border-white/10 p-3 rounded-2xl text-xs text-white outline-none focus:border-inda-blue transition-colors cursor-pointer"
  //               value={docType}
  //               onChange={(e) => setDocType(e.target.value)}
  //             >
  //               {COUNTRY_IDENTITY_CONFIG[selectedCountry].documents.map((doc) => (
  //                 <option key={doc.id} value={doc.id}>{doc.label}</option>
  //               ))}
  //             </select>
  //           </div>
  //         </div>

  //         {/* FILA 2: VALOR (ANCHO COMPLETO PERO DENTRO DEL MAX-W-2XL) */}
  //         <div className="space-y-1">
  //           <label className="text-[10px] text-zinc-500 font-bold ml-2 uppercase tracking-tighter">Document ID Value</label>
  //           <div className="relative">
  //             <input
  //               type={COUNTRY_IDENTITY_CONFIG[selectedCountry].documents.find(d => d.id === docType)?.isNumeric ? "text" : "text"}
  //               inputMode={COUNTRY_IDENTITY_CONFIG[selectedCountry].documents.find(d => d.id === docType)?.isNumeric ? "numeric" : "text"}
  //               placeholder={COUNTRY_IDENTITY_CONFIG[selectedCountry].documents.find(d => d.id === docType)?.placeholder}
  //               className="w-full bg-black/60 border border-white/10 p-4 rounded-2xl text-lg font-mono text-inda-blue outline-none focus:border-inda-blue/50 placeholder:text-zinc-700 tracking-wider"
  //               value={docValue}
  //               onChange={handleValueChange}
  //             />
  //             {/* Contador de dígitos visual */}
  //             {COUNTRY_IDENTITY_CONFIG[selectedCountry].documents.find(d => d.id === docType)?.maxDigits && (
  //               <div className="absolute right-4 top-1/2 -translate-y-1/2 text-[9px] font-bold text-zinc-600 bg-white/5 px-2 py-1 rounded-md">
  //                 {docValue.length} / {COUNTRY_IDENTITY_CONFIG[selectedCountry].documents.find(d => d.id === docType)?.maxDigits}
  //               </div>
  //             )}
  //           </div>
  //         </div>

  //         {/* COMPONENTE DIGITAL PRESENCE ABAJO */}
  //         <div className="pt-4 border-t border-white/5">
  //           <DigitalPresenceFields data={presence} onChange={setPresence} nameLabel="Artistic Name" />
  //         </div>

  //         <Button
  //           onClick={handleInternalSubmit}
  //           className="w-full bg-inda-blue! text-white! py-4 rounded-2xl font-black uppercase tracking-widest text-[11px] shadow-lg shadow-inda-blue/10"
  //           disabled={isSaving || !docValue || !presence.name}
  //         >
  //           {isSaving ? "Verifying..." : "Submit Application"}
  //         </Button>
  //       </div>
  //     </div>

  //     {/* COMPONENTE REUTILIZABLE: Digital Presence (Artistic Name + Networks) */}
  //     <DigitalPresenceFields
  //       data={presence}
  //       onChange={setPresence}
  //       nameLabel="Artistic Name"
  //     />

  //     <Button
  //       onClick={handleInternalSubmit}
  //       className="w-full bg-inda-blue! text-white! py-4 rounded-2xl font-black uppercase tracking-widest text-xs"
  //       disabled={isSaving || !formData.govIdValue || !presence.name}
  //     >
  //       {isSaving ? "Processing Request..." : "Submit Creator Application"}
  //     </Button>
  //   </div>
  // );
};

export default CreatorForm;