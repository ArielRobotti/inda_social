import React, { useEffect, useState } from 'react';
import { Camera, Save, Shield, User as UserIcon } from 'lucide-react';
import Button from '../components/Button';
import { useSession } from '@/context/SessionContext';
import { useBackend } from '@/hooks/useBackend';
import { compressAndConvertImage } from "../utils/imageManager"
import { useNavigate } from 'react-router';
import { toast } from 'sonner';


const ProfileSettings = () => {
  const { user, role, loading, updateProfile, refreshSession } = useSession();
  const navigate = useNavigate()
  const [isInitialCheckDone, setIsInitialCheckDone] = useState(false);
  const [selectedRole, setSelectedRole] = useState<string | null>(null);
  const [creatorForm, setCreatorForm] = useState({
    portfolio: '',
    webSite: '',
    govIdType: 'passport' as 'passport' | 'rfc' | 'ine' | 'other',
    govIdValue: ''
  });
  const { getBackendActor } = useBackend()


  useEffect(() => {
    const syncSession = async () => {
      // 1. Si no hay usuario y no está cargando, intentamos refrescar
      if (!user && !loading) {
        try {
          const userResult = await refreshSession();
          console.log(userResult)
          if (user) {
            setIsInitialCheckDone(true)
          }
        } catch (error) {
          console.error("Error al refrescar sesión:", error);
        }
      } else if (user) {
        // Si el usuario ya existe, el check está listo
        setIsInitialCheckDone(true);
      }
    };

    syncSession();
  }, [user, loading, refreshSession]);

  useEffect(() => {
    // Solo redirigimos si:
    // - Ya terminó de cargar el provider (!loading)
    // - Ya intentamos el refresh inicial (isInitialCheckDone)
    // - Y el usuario sigue siendo null
    if (!loading && isInitialCheckDone && !user) {
      console.log("Acceso denegado: Redirigiendo a Home");
      navigate("/");
    }
  }, [loading, isInitialCheckDone, user, navigate]);


  const [formData, setFormData] = useState({
    firstName: '',
    lastName: '',
    bio: '',
    email: '',
    avatar: null as File | null,
  });

  // Este efecto rellena el formulario cuando el usuario finalmente carga
  useEffect(() => {
    if (user) {
      setFormData({
        firstName: user.firstName || '',
        lastName: user.lastName || '',
        bio: user.bio || '',
        email: user.email?.[0] || '', // Manejo del Option [string] de Motoko
        avatar: null
      });
    }
  }, [user]);

  const [isSaving, setIsSaving] = useState(false);

  const handleSave = async () => {
    setIsSaving(true);
    const [avatar, thumbnail] = formData.avatar
      ? [[await compressAndConvertImage(formData.avatar, 256)] as [Uint8Array] | [], [await compressAndConvertImage(formData.avatar, 12)] as [Uint8Array] | []]
      : [[] as [Uint8Array] | [], [] as [Uint8Array] | []]
    console.log(formData.avatar)
    const updatePayload = {
      firstName: formData.firstName,
      lastName: formData.lastName,
      bio: formData.bio,
      email: formData.email ? [formData.email] as [string] | [] : [] as [string] | [], // Volvemos al formato ?Text
      avatar: avatar,
      thumbnail: thumbnail,
      // El avatar requiere conversión de File a Blob/Uint8Array
      metadata: [] // Tus metadatos personalizados
    };

    const updateResponse = await updateProfile(updatePayload);
    console.log(updateResponse)
    setIsSaving(false);
  };

  const handleRequestCreator = async () => {
    setIsSaving(true);
    try {
      const backend = await getBackendActor();

      // Mapeo del GovernmentID según la selección
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      let govId: any;
      if (creatorForm.govIdType === 'passport') govId = { passport: creatorForm.govIdValue };
      else if (creatorForm.govIdType === 'rfc') govId = { rfc: BigInt(creatorForm.govIdValue) };
      else if (creatorForm.govIdType === 'ine') govId = { ine: BigInt(creatorForm.govIdValue) };
      else govId = { other: { k: "custom", v: creatorForm.govIdValue } };

      const creatorPayload = {
        portfolio: creatorForm.portfolio ? [creatorForm.portfolio] : [],
        webSite: creatorForm.webSite ? [creatorForm.webSite] : [],
        governmentID: [govId], // Array<GovernmentID>
        extendedData: [] // Metadata vacía por ahora
      };

      const result = await backend.requestCreatorProfile(creatorPayload);

      if ("Ok" in result) {
        toast.success("Solicitud de creador enviada. ID: " + result.Ok);
        await refreshSession();
      } else {
        toast.error("Error: " + result.Err);
      }
    } catch (error) {
      console.error(error);
    } finally {
      setIsSaving(false);
    }
  };

  return (
    <div className="max-w-5xl mx-auto pt-24 px-6 pb-20">
      <header className="mb-10">
        <h1 className="text-4xl font-black text-white tracking-tight">Settings</h1>
        <p className="text-zinc-500 mt-2">Manage your sovereign identity on Indasocial.</p>
      </header>

      <div className="grid grid-cols-1 lg:grid-cols-3 gap-8">

        {/* Columna Izquierda: Avatar & Status */}
        <div className="lg:col-span-1 space-y-6">
          <div className="bg-zinc-900/50 border border-white/5 rounded-3xl p-8 backdrop-blur-xl flex flex-col items-center text-center">
            <div className="relative group">
              <div className="w-32 h-32 rounded-full bg-linear-to-tr from-inda-blue to-inda-purple p-1">
                <div className="w-full h-full rounded-full bg-zinc-900 flex items-center justify-center overflow-hidden border-4 border-zinc-900">
                  {/* Aquí iría la lógica para mostrar el blob del avatar actual */}
                  <span className="text-4xl font-bold text-white opacity-40">
                    {user?.firstName[0]}{user?.lastName[0]}
                  </span>
                </div>
              </div>
              <label className="absolute bottom-0 right-0 p-2 bg-inda-blue rounded-full cursor-pointer hover:scale-110 transition-transform shadow-lg">
                <Camera className="w-5 h-5 text-white" />
                {/* <input 
                  type="file" 
                  className="hidden" 
                  accept="image/*" 
                  onChange={(e) => {
                    const file = e.target.files?.[0];
                    if (file) {
                      setFormData({ ...formData, avatar: file });
                    }
                  }}
                /> */}
              </label>
            </div>

            <div className="mt-6">
              <h3 className="text-xl font-bold text-white">{formData.firstName} {formData.lastName}</h3>
              <span className="text-xs font-mono text-inda-blue/80 uppercase tracking-widest">
                Identity Verified
              </span>
            </div>
          </div>
        </div>

        {/* Columna Derecha: Formulario EditableData */}
        <div className="lg:col-span-2">
          <div className="bg-zinc-900/50 border border-white/5 rounded-3xl p-8 backdrop-blur-xl space-y-8">

            <section className="space-y-6">
              <div className="flex items-center gap-2 text-inda-blue">
                <UserIcon className="w-5 h-5" />
                <h2 className="font-bold uppercase tracking-wider text-sm">Personal Information</h2>
              </div>

              <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                <div className="space-y-2">
                  <label className="text-xs text-zinc-500 ml-1">First Name</label>
                  <input
                    type="text"
                    value={formData.firstName}
                    onChange={(e) => setFormData({ ...formData, firstName: e.target.value })}
                    className="w-full bg-white/5 border border-white/10 rounded-xl px-4 py-3 text-white focus:border-inda-blue outline-none transition-all"
                  />
                </div>
                <div className="space-y-2">
                  <label className="text-xs text-zinc-500 ml-1">Last Name</label>
                  <input
                    type="text"
                    value={formData.lastName}
                    onChange={(e) => setFormData({ ...formData, lastName: e.target.value })}
                    className="w-full bg-white/5 border border-white/10 rounded-xl px-4 py-3 text-white focus:border-inda-blue outline-none transition-all"
                  />
                </div>
              </div>

              <div className="space-y-2">
                <label className="text-xs text-zinc-500 ml-1">Email Address (Optional)</label>
                <input
                  type="email"
                  value={formData.email}
                  onChange={(e) => setFormData({ ...formData, email: e.target.value })}
                  className="w-full bg-white/5 border border-white/10 rounded-xl px-4 py-3 text-white focus:border-inda-blue outline-none transition-all"
                  placeholder="fiodor@dostoyevsqui.com"
                />
              </div>

              <div className="space-y-2">
                <label className="text-xs text-zinc-500 ml-1">Bio</label>
                <textarea
                  rows={4}
                  value={formData.bio}
                  onChange={(e) => setFormData({ ...formData, bio: e.target.value })}
                  className="w-full bg-white/5 border border-white/10 rounded-xl px-4 py-3 text-white focus:border-inda-blue outline-none transition-all resize-none"
                />
              </div>
            </section>

            <div className="pt-6 border-t border-white/5 flex justify-end">
              <Button
                onClick={handleSave}
                disabled={isSaving}
                className="px-10 py-3 flex items-center gap-2"
              >
                <Save className="w-4 h-4" />
                {isSaving ? "Synchronizing..." : "Save Changes"}
              </Button>
            </div>

          </div>
        </div>
      </div>
      {/* Estado de Solicitud Pendiente */}
      {!role && user?.roleRequestedOrAsigned && (
        <div className="mt-6 flex items-center gap-4 p-4 bg-amber-500/10 border border-amber-500/20 rounded-2xl animate-pulse">
          <div className="flex-shrink-0 w-10 h-10 bg-amber-500/20 rounded-full flex items-center justify-center">
            <Shield className="w-5 h-5 text-amber-500" />
          </div>
          <div>
            <h4 className="text-sm font-bold text-amber-500 uppercase tracking-tight">
              Application Under Review
            </h4>
            <p className="text-[10px] text-amber-500/70 leading-tight">
              Your application for a specialized profile is being processed in the canister. 
              We will notify you when the role is assigned
            </p>
          </div>
        </div>
      )}
      {user && !user.roleRequestedOrAsigned && (
        <div className="space-y-6 pt-8 border-t border-white/5">
          <div className="flex items-center gap-2 text-inda-purple">
            <Shield className="w-5 h-5" />
            <h2 className="font-bold uppercase tracking-wider text-xs">Request Specialized Profile</h2>
          </div>

          <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
            {/* Botón Creador */}
            <button
              onClick={() => setSelectedRole('creator')}
              className={`p-4 rounded-2xl border transition-all text-left ${selectedRole === 'creator' ? "bg-inda-blue/10 border-inda-blue" : "bg-white/5 border-white/10"
                }`}
            >
              <span className="text-2xl mb-2 block">🎨</span>
              <h4 className="font-bold text-white text-sm">Creator</h4>
              <p className="text-[10px] text-zinc-500">Assets 3D & Content</p>
            </button>

            {/* Otros roles deshabilitados por ahora */}
            <div className="p-4 rounded-2xl border border-white/5 opacity-40 cursor-not-allowed">
              <span className="text-2xl mb-2 block">🏢</span>
              <h4 className="font-bold text-white text-sm">Brand</h4>
              <p className="text-[10px] text-zinc-500 italic">Coming soon</p>
            </div>
            <div className="p-4 rounded-2xl border border-white/5 opacity-40 cursor-not-allowed">
              <span className="text-2xl mb-2 block">🤝</span>
              <h4 className="font-bold text-white text-sm">Partner</h4>
              <p className="text-[10px] text-zinc-500 italic">Coming soon</p>
            </div>
          </div>

          {/* Formulario extra si selecciona Creador */}
          {selectedRole === 'creator' && (
            <div className="bg-inda-blue/5 border border-inda-blue/20 rounded-2xl p-6 space-y-4 animate-in fade-in slide-in-from-top-2">
              <h3 className="text-sm font-bold text-white mb-4">Creator Application Details</h3>

              <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                <input
                  placeholder="Portfolio URL"
                  className="bg-zinc-900 border border-white/10 p-3 rounded-xl text-sm outline-none focus:border-inda-blue"
                  onChange={(e) => setCreatorForm({ ...creatorForm, portfolio: e.target.value })}
                />
                <input
                  placeholder="Website URL"
                  className="bg-zinc-900 border border-white/10 p-3 rounded-xl text-sm outline-none focus:border-inda-blue"
                  onChange={(e) => setCreatorForm({ ...creatorForm, webSite: e.target.value })}
                />
              </div>

              <div className="flex flex-col md:flex-row gap-4">
                <select
                  className="bg-zinc-900 border border-white/10 p-3 rounded-xl text-sm text-zinc-400 outline-none"
                  value={creatorForm.govIdType}
                  // eslint-disable-next-line @typescript-eslint/no-explicit-any
                  onChange={(e: any) => setCreatorForm({ ...creatorForm, govIdType: e.target.value })}
                >
                  <option value="passport">Passport</option>
                  <option value="rfc">RFC (BigInt)</option>
                  <option value="ine">INE (BigInt)</option>
                </select>
                <input
                  placeholder="Document ID Value"
                  className="flex-1 bg-zinc-900 border border-white/10 p-3 rounded-xl text-sm outline-none focus:border-inda-blue"
                  onChange={(e) => setCreatorForm({ ...creatorForm, govIdValue: e.target.value })}
                />
              </div>

              <Button
                onClick={handleRequestCreator}
                className="w-full bg-inda-blue! text-white! py-3 rounded-xl font-bold"
                disabled={isSaving || !creatorForm.govIdValue}
              >
                Submit Creator Request
              </Button>
            </div>
          )}
        </div>
      )}
    </div>
  );
};

export default ProfileSettings