import React, { useState } from 'react';
import { useSession } from "../context/SessionContext";
import { ConnectWallet, ConnectWalletDropdownMenuDisconnectItem } from "@nfid/identitykit/react";
import Button from './Button';
import { X, Mail, FileText } from 'lucide-react';
import { cn } from '@/lib/utils';


// eslint-disable-next-line @typescript-eslint/no-explicit-any
const MyCustomDisconnectButton = (props: any) => {
  const { disconnect, ...restProps } = props;
  return (
    <div 
      className="p-2 h-10 w-10 bg-zinc-800 rounded-full text-inda-blue"
      onClick={disconnect}
    >
      <X className="w-6 h-6" />
    </div>
  );
};

const RegistrationModal = () => {
  const { signUp } = useSession();
  const [formData, setFormData] = useState({
    firstName: '',
    lastName: '',
    email: '',
    bio: ''
  });
  const [isSubmitting, setIsSubmitting] = useState(false);
  const maxLengthBio = 200;

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setIsSubmitting(true);
    
    try {
      // Convertimos el email al formato Option [string] que espera el backend
      const formattedData = {
        firstName: formData.firstName.trim(),
        lastName: formData.lastName.trim().charAt(0).toUpperCase() + formData.lastName.trim().slice(1).toLowerCase(),
        bio: formData.bio,
        email: formData.email ? [formData.email] as [string] : [] as [string] | []
      };
      
      await signUp(formattedData);
    } catch (error) {
      console.error("Error en el registro:", error);
    } finally {
      setIsSubmitting(false);
    }
  };

  return (
    <div className="fixed inset-0 z-[100] flex items-center justify-center p-4 bg-black/60 backdrop-blur-sm">
      <div className="bg-zinc-900 border border-zinc-800 w-full max-w-md rounded-2xl overflow-hidden shadow-2xl">
        {/* Header */}
        <div className="p-6 border-b border-zinc-800 flex justify-between bg-gradient-to-r from-inda-blue/10 to-transparent">
          <div>
            <h2 className="text-2xl font-bold text-white">Join the Indasocial</h2>
            <p className="text-zinc-400 text-sm">Complete your profile</p>
          </div>
          <ConnectWallet dropdownMenuComponent={MyCustomDisconnectButton} />
        </div>

        {/* Formulario */}
        <form onSubmit={handleSubmit} className="p-6 space-y-4">
          <div className="grid grid-cols-2 gap-4">
            <div className="space-y-1">
              <label className="text-xs font-medium text-zinc-500 uppercase ml-1">First Name</label>
              <input
                required
                type="text"
                className="w-full bg-zinc-950 border border-zinc-800 rounded-xl px-4 py-2.5 text-white focus:border-inda-blue focus:ring-1 focus:ring-inda-blue outline-none transition-all"
                placeholder="Fiodor"
                value={formData.firstName}
                onChange={(e) => setFormData({...formData, firstName: e.target.value})}
              />
            </div>
            <div className="space-y-1">
              <label className="text-xs font-medium text-zinc-500 uppercase ml-1">Last Name</label>
              <input
                required
                type="text"
                className="w-full bg-zinc-950 border border-zinc-800 rounded-xl px-4 py-2.5 text-white focus:border-inda-blue focus:ring-1 focus:ring-inda-blue outline-none transition-all"
                placeholder="Dostoyevsqui"
                value={formData.lastName}
                onChange={(e) => setFormData({...formData, lastName: e.target.value})}
              />
            </div>
          </div>

          <div className="space-y-1">
            <label className="text-xs font-medium text-zinc-500 uppercase ml-1">Email (Optional)</label>
            <div className="relative">
              <Mail className="absolute left-3 top-3 w-4 h-4 text-zinc-500" />
              <input
                type="email"
                className="w-full bg-zinc-950 border border-zinc-800 rounded-xl pl-10 pr-4 py-2.5 text-white focus:border-inda-blue outline-none transition-all"
                placeholder="name@example.com"
                value={formData.email}
                onChange={(e) => setFormData({...formData, email: e.target.value.toLowerCase()})}
              />
            </div>
          </div>

          <div className="space-y-1">
            <label className="text-xs font-medium text-zinc-500 uppercase ml-1">Bio</label>
            <div className="relative">
              <FileText className="absolute left-3 top-3 w-4 h-4 text-zinc-500" />
              <textarea
                required
                rows={3}
                maxLength={maxLengthBio}
                className="w-full bg-zinc-950 border border-zinc-800 rounded-xl pl-10 pr-4 py-2.5 text-white focus:border-inda-blue outline-none transition-all resize-none"
                placeholder="Tell us something about yourself..."
                value={formData.bio}
                onChange={(e) => setFormData({...formData, bio: e.target.value})}
              />
            </div>
            <div className="flex justify-end pr-2">
                <span className={cn(
                "text-[12px] font-mono transition-colors",
                (maxLengthBio - formData.bio.length) <= 10 ? "text-red-500 font-bold" : "text-zinc-500"
                )}>
                {maxLengthBio - formData.bio.length} / {maxLengthBio}
                </span>
            </div>
          </div>

          <Button 
            type="submit" 
            variant="primary" 
            className="w-full py-3 mt-4" 
            disabled={isSubmitting}
          >
            {isSubmitting ? "Registering..." : "Sign Up"}
          </Button>
        </form>
        {/* <ConnectWallet dropdownMenuComponent={MyCustomButton} /> */}
      </div>
    </div>
  );
};

export default RegistrationModal;