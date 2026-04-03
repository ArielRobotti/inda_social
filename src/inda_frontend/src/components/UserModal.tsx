import React, { useEffect, useState } from 'react';
import { X, Mail, Fingerprint, Calendar, Star, Info } from 'lucide-react';
import { User } from '@/declarations/backend/backend.did';

interface UserModalProps {
  user: User;
  onClose: () => void;
}

const UserModal = ({ user, onClose }: UserModalProps) => {
  // Helper para convertir los Blobs de Motoko en URLs
  const getImageUrl = (blobArray: [] | [Uint8Array | number[]]) => {
    if (blobArray.length === 0) return null;
    const blob = new Blob([new Uint8Array(blobArray[0])], { type: 'image/png' });
    return URL.createObjectURL(blob);
  };

  const avatarUrl = getImageUrl(user.avatar);
  console.log(user)

  return (
    <div className="fixed inset-0 z-50 flex items-center justify-center p-4 bg-black/80 backdrop-blur-sm animate-in fade-in duration-200">
      <div className="bg-zinc-900 border border-white/10 w-full max-w-lg rounded-[2.5rem] overflow-hidden shadow-2xl relative">
        
        {/* Header con Imagen de Fondo / Banner */}
        <div className="h-24 bg-linear-to-r from-inda-blue/20 to-inda-purple/20 w-full" />

        <button 
          onClick={onClose}
          className="absolute top-4 right-4 p-2 bg-black/20 hover:bg-white/10 rounded-full text-white/50 hover:text-white transition-colors"
        >
          <X size={20} />
        </button>

        <div className="px-8 pb-8 -mt-12">
          {/* Avatar */}
          <div className="w-24 h-24 rounded-3xl bg-zinc-800 border-4 border-zinc-900 overflow-hidden mb-4 shadow-xl">
            {avatarUrl ? (
              <img src={avatarUrl} alt="Avatar" className="w-full h-full object-cover" />
            ) : (
              <div className="w-full h-full flex items-center justify-center text-3xl font-black text-white/20">
                {user.firstName[0]}
              </div>
            )}
          </div>

          {/* Info Básica */}
          <div className="space-y-1 mb-6">
            <h2 className="text-2xl font-black text-white">{user.firstName} {user.lastName}</h2>
            <div className="flex items-center gap-2 text-inda-blue text-xs font-mono">
              <Fingerprint size={14} />
              <span className="truncate">{user.principal.toText()}</span>
            </div>
          </div>

          {/* Stats Grid */}
          <div className="grid grid-cols-2 gap-3 mb-6">
            <div className="bg-white/5 p-3 rounded-2xl border border-white/5">
              <p className="text-[10px] text-zinc-500 uppercase font-bold mb-1">Scoring</p>
              <div className="flex items-center gap-1 text-amber-400 font-bold">
                <Star size={14} fill="currentColor" />
                {user.scoring.toString()}
              </div>
            </div>
            <div className="bg-white/5 p-3 rounded-2xl border border-white/5">
              <p className="text-[10px] text-zinc-500 uppercase font-bold mb-1">Last Activity</p>
              <div className="flex items-center gap-1 text-zinc-300 text-xs">
                <Calendar size={14} />
                {new Date(Number(user.lastActivity) / 1_000_000).toLocaleDateString()}
              </div>
            </div>
          </div>

          {/* Bio & Details */}
          <div className="space-y-4">
            <div className="bg-white/5 p-4 rounded-2xl border border-white/5">
              <div className="flex items-center gap-2 text-zinc-400 mb-2">
                <Info size={16} />
                <span className="text-xs font-bold uppercase">Biography</span>
              </div>
              <p className="text-sm text-zinc-300 leading-relaxed italic">
                "{user.bio || 'No bio provided'}"
              </p>
            </div>

            {user.email[0] && (
              <div className="flex items-center gap-3 px-4 py-3 bg-inda-blue/5 border border-inda-blue/10 rounded-xl">
                <Mail size={16} className="text-inda-blue" />
                <span className="text-sm text-white/80">{user.email[0]}</span>
              </div>
            )}
          </div>
        </div>
      </div>
    </div>
  );
};

export default UserModal