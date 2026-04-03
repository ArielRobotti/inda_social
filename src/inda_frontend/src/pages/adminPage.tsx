// /* eslint-disable @typescript-eslint/no-explicit-any */
import React, { useEffect, useState } from 'react';
import { ShieldCheck, XCircle, ExternalLink, User as UserIcon, ChevronDown, Globe, Mail, MapPin, FileText } from 'lucide-react';
import { useSession } from '@/context/SessionContext';
import { Request, User, MetadataPart, Value } from '@/declarations/backend/backend.did';
import { toast } from 'sonner';
import { Principal } from '@dfinity/principal';
import UserModal from "../components/UserModal";
import { SOCIAL_CONFIG } from '@/utils/socialConfigs';

const formatRequestDate = (nanos: bigint) => {
  // id/1_000_000 para pasar de nanosegundos a milisegundos para Date()
  const date = new Date(Number(nanos) / 1_000_000);
  const ahora = new Date();
  const isToday = date.toDateString() === ahora.toDateString();

  if (isToday) {
    return `Today at ${date.toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' })}`;
  }
  return date.toLocaleDateString([], { day: '2-digit', month: 'short', year: 'numeric' });
};

const AdminPage = () => {
  const [requests, setRequests] = useState<[Principal, Request][]>([]);
  const [loading, setLoading] = useState(true);
  const { backend } = useSession();
  const [selectedUser, setSelectedUser] = useState<User | null>(null);
  const [isFetchingUser, setIsFetchingUser] = useState(false);
  const [expandedId, setExpandedId] = useState<string | null>(null);

  const fetchRequests = async () => {
    try {
      const result = await backend.getRequestRole();
      if ("Ok" in result) {
        setRequests(result.Ok);
      }
    } catch (error) {
      toast.error("Error fetching requests");
    } finally {
      setLoading(false);
    }
  };

  useEffect(() => { fetchRequests(); }, []);

  const handleViewUser = async (principal: Principal) => {
    setIsFetchingUser(true);
    try {
      const result = await backend.getUser(principal);
      if ("Ok" in result) {
        setSelectedUser(result.Ok);
      } else {
        toast.error("User not found");
      }
    } catch (error) {
      toast.error("Failed to fetch user base data");
    } finally {
      setIsFetchingUser(false);
    }
  };

  // Helper para extraer valores de extendedData o metadata
  const getMetaValue = (metadata: MetadataPart[], key: string): Value => {
    const item = metadata.find(m => m.key === key);
    return item ? item.value : null;
  };

  const handleAcceptRoleRequest = async (user: Principal) => {
    console.log(user)
    const result = await backend.acceptRoleRequest(user)
    console.log(result)
  };

 const handleRejectRoleRequest = async (user: Principal) => {
    const result = await backend.rejectRoleRequest(user)
  };

  const renderValue = (value: Value): string => {
    if (!value) return "";
    if ('Text' in value) return value.Text;
    if ('Int' in value) return value.Int.toString();
    if ('Nat' in value) return value.Nat.toString();
    return "";
  };

  if (loading) return <div className="p-20 text-center animate-pulse text-zinc-500">Loading requests...</div>;

  return (
    <div className="max-w-[90%] mx-auto pt-24 px-6 pb-20">
      <header className="mb-10 w-full flex justify-between items-end">
        <div className='w-full'>
          <h1 className="text-3xl text-center font-black text-white">Admin Panel</h1>
          <div className="h-[2px] w-full my-8 bg-linear-to-r from-transparent via-violet-400 to-transparent blur-[0.5px]" />
          <h1 className="text-3xl font-black text-white uppercase tracking-tighter">Pending Authorizations</h1>
          <p className="text-zinc-500 text-sm">Review and verify decentralized identity requests.</p>
        </div>
        <div className="w-64 bg-zinc-900 border border-white/5 px-4 py-2 rounded-full text-[10px] font-mono text-inda-blue text-center">
          {requests.length} PENDING REQUESTS
        </div>
      </header>

      <div className="space-y-4">
        {requests.map(([principal, req]) => {
          const isExpanded = expandedId === req.id.toString();
          const creatorData = 'NewCreator' in req.kind ? req.kind.NewCreator : null;
          const extended = creatorData?.extendedData || [];

          // Datos para Vista Previa
          const artisticName = renderValue(getMetaValue(extended, "artisticName")) || "Unknown Creator";
          const country = renderValue(getMetaValue(extended, "country")) || "??";
          const category = renderValue(getMetaValue(extended, "category"));

          return (
            <div
              key={req.id.toString()}
              className={`bg-zinc-900/40 border transition-all duration-300 rounded-[2rem] overflow-hidden ${isExpanded ? 'border-inda-blue/40 bg-zinc-900/80 shadow-2xl scale-[1.01]' : 'border-white/5 hover:border-white/10'
                }`}
            >
              {/* CABECERA DE LA FILA (VISTA PREVIA) */}
              <div
                onClick={() => setExpandedId(isExpanded ? null : req.id.toString())}
                className="p-6 cursor-pointer flex items-center justify-between gap-4"
              >
                <div className="flex items-center gap-4 flex-1">
                  <div className={`w-12 h-12 rounded-full flex items-center justify-center transition-all ${isExpanded ? 'bg-inda-blue text-white shadow-[0_0_20px_rgba(0,210,255,0.3)]' : 'bg-white/5 text-zinc-500'}`}>
                    <UserIcon size={24} />
                  </div>
                  <div className="flex flex-col">
                    <div className="flex items-center gap-2">
                      <span className="text-lg font-black text-white tracking-tight">{artisticName}</span>
                      {category && (
                        <span className="px-2 py-0.5 bg-inda-blue/10 border border-inda-blue/20 rounded text-[9px] text-inda-blue font-bold uppercase">
                          {category}
                        </span>
                      )}
                    </div>
                    <span className="text-[10px] font-mono text-zinc-500 truncate max-w-[300px]">
                      {principal.toString()}
                    </span>
                  </div>
                </div>

                <div className="flex items-center gap-8">
                  <div className="hidden sm:flex flex-col items-end">
                    <span className="text-[9px] text-zinc-500 font-bold uppercase tracking-widest">Region</span>
                    <div className="flex items-center gap-1 text-zinc-200">
                      <MapPin size={12} className="text-inda-purple" />
                      <span className="text-sm font-bold">{country}</span>
                    </div>
                  </div>

                  <div className="text-right min-w-[120px]">
                    <span className="text-[9px] text-zinc-500 font-bold uppercase block tracking-widest">Requested</span>
                    <span className="text-xs font-medium text-zinc-300">{formatRequestDate(req.id)}</span>
                  </div>

                  <ChevronDown className={`text-zinc-500 transition-transform duration-500 ${isExpanded ? 'rotate-180' : ''}`} size={20} />
                </div>
              </div>

              {/* CUERPO EXPANDIBLE (VISTA AMPLIADA) */}
              {isExpanded && (
                <div className="px-8 pb-8 animate-in slide-in-from-top-4 duration-500">
                  <div className="h-[1px] w-full bg-linear-to-r from-transparent via-white/10 to-transparent mb-8" />

                  <div className="grid grid-cols-1 lg:grid-cols-3 gap-8">

                    {/* COLUMNA 1: IDENTIDAD LEGAL */}
                    <div className="space-y-4">
                      <h4 className="text-[11px] font-black text-zinc-400 uppercase tracking-[0.2em] flex items-center gap-2">
                        <ShieldCheck size={14} className="text-inda-blue" /> Identity Verification
                      </h4>
                      <div className="bg-black/30 rounded-3xl p-5 border border-white/5 space-y-4">
                        {creatorData?.governmentID.map((doc, idx) => (
                          <div key={idx} className="flex flex-col p-3 bg-white/5 rounded-xl border border-white/5">
                            <span className="text-[9px] text-zinc-500 font-bold uppercase">{doc.docType}</span>
                            <span className="text-sm font-mono text-inda-blue break-all">{doc.value}</span>
                          </div>
                        ))}
                        <div className="grid grid-cols-2 gap-4 pt-2">
                          <div>
                            <span className="text-[9px] text-zinc-500 font-bold uppercase block mb-1">Gender</span>
                            <span className="text-xs text-zinc-200 capitalize">{renderValue(getMetaValue(extended, "gender"))}</span>
                          </div>
                          <div>
                            <span className="text-[9px] text-zinc-500 font-bold uppercase block mb-1">Birth Date</span>
                            <span className="text-xs text-zinc-200 font-mono">{renderValue(getMetaValue(extended, "birthDate"))}</span>
                          </div>
                        </div>
                      </div>
                    </div>

                    {/* COLUMNA 2: DIGITAL ECOSYSTEM */}
                    <div className="space-y-4">
                      <h4 className="text-[11px] font-black text-zinc-400 uppercase tracking-[0.2em] flex items-center gap-2">
                        <Globe size={14} className="text-inda-purple" /> Digital Ecosystem
                      </h4>
                      <div className="bg-black/30 rounded-3xl p-5 border border-white/5 space-y-5">
                        <div className="space-y-3">
                          <span className="text-[9px] text-zinc-500 font-bold uppercase tracking-widest">Digital Footprint</span>
                          <div className="flex flex-wrap gap-3">
                            {(() => {
                              const networksVal = getMetaValue(extended, "networks");
                              if (networksVal && 'Map' in networksVal) {
                                // eslint-disable-next-line @typescript-eslint/no-explicit-any
                                return networksVal.Map.map(([netId, userVal]: any) => {
                                  const config = SOCIAL_CONFIG[netId];
                                  console.log(config)
                                  const username = renderValue(userVal);

                                  // Reconstruimos la URL si existe el pattern/base en tu config
                                  // Si tu SOCIAL_CONFIG tiene una propiedad 'url', úsala, sino la armamos:
                                  const profileUrl = config?.baseUrl
                                    ? `${config.baseUrl}${username}`
                                    : `https://${netId}.com/${username}`;

                                  return (
                                    <a
                                      key={netId}
                                      href={profileUrl}
                                      target="_blank"
                                      rel="noopener noreferrer"
                                      title={`${config?.label || netId}: @${username}`}
                                      className="group relative flex items-center justify-center w-10 h-10 bg-white/40 border border-white/10 rounded-xl hover:border-inda-blue/50 hover:bg-white transition-all duration-300"
                                    >
                                      {/* Icono Principal */}
                                      <div className="h-6 w-6 text-zinc-400 group-hover:text-inda-blue transition-colors">
                                        {config?.icon || <Globe size={18} />}
                                      </div>

                                      {/* Indicador de enlace externo mini */}
                                      <div className="absolute -top-1 -right-1 w-3 h-3 bg-inda-blue rounded-full flex items-center justify-center opacity-0 group-hover:opacity-100 transition-opacity">
                                        <ExternalLink size={6} className="text-white" />
                                      </div>
                                    </a>
                                  );
                                });
                              }
                              return <span className="text-xs text-zinc-600 italic">No social profiles linked</span>;
                            })()}
                          </div>
                        </div>

                        <div className="space-y-2 pt-2">
                          <span className="text-[9px] text-zinc-500 font-bold uppercase">Official Links</span>
                          <div className="flex flex-col gap-2">
                            {['portfolio', 'webSite'].map(key => {
                              const url = renderValue(getMetaValue(extended, key));
                              if (!url) return null;
                              return (
                                <a key={key} href={url} target="_blank" className="flex items-center justify-between p-2 bg-white/5 hover:bg-inda-blue/10 border border-white/5 rounded-xl transition-colors group">
                                  <span className="text-[10px] text-zinc-400 uppercase font-bold group-hover:text-inda-blue">{key}</span>
                                  <ExternalLink size={12} className="text-zinc-600 group-hover:text-inda-blue" />
                                </a>
                              );
                            })}
                          </div>
                        </div>
                      </div>
                    </div>

                    {/* COLUMNA 3: ACCIONES */}
                    <div className="flex flex-col justify-between py-2">
                      <div className="space-y-4">
                        <div className="p-6 bg-linear-to-br from-white/5 to-transparent rounded-[2rem] border border-white/5 shadow-inner">
                          <p className="text-[10px] text-zinc-500 text-center uppercase font-black tracking-[0.3em] mb-6">Reviewer Decision</p>
                          <div className="flex flex-col gap-3">
                            <button 
                              onClick={() => handleAcceptRoleRequest(principal)}
                              className="group w-full py-4 bg-emerald-500 text-white rounded-2xl hover:bg-emerald-600 transition-all flex items-center justify-center gap-3 text-xs font-black uppercase tracking-widest shadow-lg shadow-emerald-500/20 active:scale-95"
                            >
                              <ShieldCheck size={18} className="group-hover:animate-bounce" /> Approve Role
                            </button>
                            <button className="w-full py-4 bg-zinc-800 text-red-500 border border-red-500/10 rounded-2xl hover:bg-red-500 hover:text-white transition-all flex items-center justify-center gap-3 text-xs font-black uppercase tracking-widest active:scale-95">
                              <XCircle size={18} /> Decline
                            </button>
                          </div>
                        </div>
                      </div>

                      <button
                        onClick={() => handleViewUser(principal)}
                        className="mt-6 flex items-center justify-center gap-2 py-3 text-[10px] text-zinc-500 hover:text-inda-blue uppercase font-black tracking-tighter transition-colors border-t border-white/5"
                      >
                        <FileText size={14} /> Full sovereign audit trail
                      </button>
                    </div>

                  </div>
                </div>
              )}
            </div>
          );
        })}

        {requests.length === 0 && (
          <div className="text-center py-24 bg-zinc-900/20 border border-dashed border-white/10 rounded-[3rem]">
            <div className="w-16 h-16 bg-white/5 rounded-full flex items-center justify-center mx-auto mb-4">
              <ShieldCheck size={32} className="text-zinc-700" />
            </div>
            <p className="text-zinc-500 font-medium italic">All requests have been processed.</p>
          </div>
        )}
      </div>

      {selectedUser && (
        <UserModal user={selectedUser} onClose={() => setSelectedUser(null)} />
      )}

      {isFetchingUser && (
        <div className="fixed inset-0 z-50 flex items-center justify-center bg-black/60 backdrop-blur-md">
          <div className="flex flex-col items-center gap-4">
            <div className="w-12 h-12 border-4 border-inda-blue border-t-transparent rounded-full animate-spin" />
            <span className="text-white font-black uppercase tracking-widest text-xs">Decrypting Identity...</span>
          </div>
        </div>
      )}
    </div>
  );
};

export default AdminPage;