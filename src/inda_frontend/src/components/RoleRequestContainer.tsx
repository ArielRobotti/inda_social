// components/settings/RoleRequestContainer.tsx
import React, { useState } from 'react';
import { Shield } from 'lucide-react';
import { toast } from 'sonner';
import CreatorForm from './forms/CreatorForm';
import { type _SERVICE } from '@/declarations/backend/backend.did';
import { ActorSubclass } from '@dfinity/agent';
// import BrandForm from './forms/BrandForm'; // Próximamente

interface props {
  backendAccessor: () => Promise<ActorSubclass<_SERVICE>>;
  onRefresh: () => void 
}

const RoleRequestContainer = ({ backendAccessor, onRefresh }: props) => {
  const [selectedRole, setSelectedRole] = useState<'creator' | 'brand' | 'partnership' | null>(null);
  const [isSaving, setIsSaving] = useState(false);

  return (
    <div className="space-y-6 mt-4 pt-4 border-t border-white/5">
      <div className="flex items-center gap-2 text-inda-purple">
        <Shield className="w-5 h-5" />
        <h2 className="font-bold uppercase tracking-wider text-xs">Request Specialized Profile</h2>
      </div>

      {/* Selector de Roles */}
      <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
        <RoleCard 
          id="creator" 
          icon="🎨" 
          title="Creator" 
          desc="3D Assets & Content" 
          active={selectedRole === 'creator'} 
          onClick={setSelectedRole} 
        />
        <RoleCard 
          id="brand" 
          icon="🏢" 
          title="Brand" 
          desc="Companies & Marketing" 
          disabled 
          onClick={setSelectedRole} 
        />
        <RoleCard 
          id="partnership" 
          icon="🤝" 
          title="Partner" 
          desc="B2B & Collaboration" 
          disabled 
          onClick={setSelectedRole} 
        />
      </div>

      {/* Renderizado Condicional del Formulario Específico */}
      {selectedRole === 'creator' && (
      <CreatorForm 
        isSaving={isSaving} 
        onSubmit={async (payload) => {
          setIsSaving(true);
          try {
            const backend = await backendAccessor(); // getBackendActor()
            const result = await backend.requestCreatorProfile(payload);
            if ("Ok" in result) {
              toast.success("Identity request sent successfully!");
              onRefresh();
            } else {
              toast.error("Canister rejected request: " + result.Err);
            }
          } catch (error) {
            toast.error("Connection failed");
          } finally {
            setIsSaving(false);
          }
        }} 
      />
    )}
    </div>
  );
};

// Sub-componente interno para las cards de rol
// eslint-disable-next-line @typescript-eslint/no-explicit-any
const RoleCard = ({ id, icon, title, desc, active, disabled, onClick }: any) => (
  <button
    disabled={disabled}
    onClick={() => onClick(id)}
    className={`p-4 rounded-2xl border transition-all text-left cursor-pointer ${
      active ? "bg-inda-blue/10 border-inda-blue" : "bg-white/5 border-white/10"
    } ${disabled ? "opacity-40 cursor-not-allowed" : "hover:border-white/20"}`}
  >
    <span className="text-2xl mb-2 block">{icon}</span>
    <h4 className="font-bold text-white text-sm">{title}</h4>
    <p className="text-[10px] text-zinc-500">{disabled ? "Coming soon" : desc}</p>
  </button>
);

export default RoleRequestContainer;