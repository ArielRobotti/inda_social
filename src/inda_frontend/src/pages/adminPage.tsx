/* eslint-disable @typescript-eslint/no-explicit-any */
import React, { useEffect, useState } from 'react';
import { ShieldCheck, XCircle, ExternalLink, User as UserIcon, FileText, Divide } from 'lucide-react';
import { useSession } from '@/context/SessionContext';
import { Request, User } from '@/declarations/backend/backend.did';
// import Button from '../components/Button';
import { toast } from 'sonner';
import { Principal } from '@dfinity/principal';
import UserModal from "../components/UserModal"

const formatRequestDate = (nanos: bigint) => {
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

    const renderMetadataValue = (value: any): string => {
        if ('Text' in value) return value.Text;
        if ('Int' in value || 'Nat' in value) return value.Int?.toString() || value.Nat?.toString();
        if ('Blob' in value) return `Blob (${value.Blob.length} bytes)`;
        return "[Complex Value]";
    };

    const fetchRequests = async () => {
        try {
            const result = await backend.getRequestRole();
            if ("Ok" in result) {
                console.log(result.Ok)
                setRequests(result.Ok);
            }
        } catch (error) {
            toast.error("Error fetching requests");
        } finally {
            setLoading(false);
        }
    };

    useEffect(() => { fetchRequests(); }, []);

    if (loading) return <div className="p-20 text-center animate-pulse text-zinc-500">Loading requests...</div>;

    return (
        <div className="max-w-[90%] mx-auto pt-24 px-6 pb-20">
            {/* Decoración: Línea de acento superior sutil */}
            <div className="absolute top-0 left-0 w-full h-[1px] bg-linear-to-r from-transparent via-inda-blue/20 to-transparent opacity-0 group-hover:opacity-100 transition-opacity" />
            <header className="mb-10 w-full flex justify-between items-end">
                <div className='w-full'>
                    <h1 className="text-3xl text-center font-black text-white"> Admin Pannel</h1>
                    <div className="h-[2px] w-full my-8 bg-linear-to-r from-transparent via-violet-400 to-transparent blur-[0.5px]" />
                    <h1 className="text-3xl font-black text-white uppercase tracking-tighter">Pending Authorizations</h1>
                    <p className="text-zinc-500 text-sm">Review and verify decentralized identity requests.</p>
                </div>
                <div className="w-50 bg-zinc-900 border border-white/5 px-4 py-2 rounded-full text-[10px] font-mono text-inda-blue">
                    {requests.length} PENDING REQUESTS
                </div>
            </header>

            <div className="space-y-4">
                {requests.map(([principal, req]) => (
                    <div key={principal.toString()} className="bg-zinc-900/40 border border-white/5 rounded-3xl p-6 backdrop-blur-md hover:border-white/10 transition-all">
                        <div className="flex justify-between items-center mb-6">
                            <div className="flex items-center gap-2">
                                <div className="w-2 h-2 rounded-full bg-inda-blue animate-pulse" />
                                <span className="text-[10px] font-black text-zinc-500 uppercase tracking-[0.2em]">
                                    System Request #{req.id.toString()}
                                </span>
                            </div>
                            
                            {/* LA FECHA MEJORADA */}
                            <div className="flex items-center gap-2 px-3 py-1 bg-white/5 border border-white/5 rounded-full">
                                <span className="text-[12px] font-mono text-zinc-300">
                                    {formatRequestDate(req.id)}
                                </span>
                            </div>
                        </div>
                        <div className="flex flex-col lg:flex-row gap-8">
                            {/* Info Principal */}
                            <div className="flex-1 space-y-4 cursor-pointer" onClick={() => handleViewUser(principal)}>
                                <div
                                    className="flex items-center gap-3"
                                    
                                >
                                    <div className="w-10 h-10 rounded-full bg-linear-to-br from-inda-blue to-inda-purple flex items-center justify-center">
                                        <UserIcon className="w-5 h-5 text-white" />
                                    </div>
                                    <div>
                                        {/* <p className="text-[10px] text-zinc-500 font-mono uppercase tracking-widest">Principal ID</p> */}
                                        <p className="text-sm font-bold text-white truncate max-w-50 lg:w-full">{principal.toString()}</p>
                                    </div>
                                </div>

                                <div className="flex gap-2">
                                    {Object.keys(req.kind).map(k => (
                                        <span key={k} className="px-3 py-1 bg-white/5 border border-white/10 rounded-full text-[10px] font-bold text-inda-blue uppercase">
                                            {k.replace('New', '')} Request
                                        </span>
                                    ))}
                                </div>
                            </div>

                            {/* Data Específica (CreatorDataInit) */}
                            <div className="flex-[2] grid grid-cols-1 md:grid-cols-2 gap-6 bg-black/20 rounded-2xl p-4">
                                {'NewCreator' in req.kind && (
                                    <>
                                        <div className="space-y-1">
                                            <p className="text-[10px] text-zinc-500 uppercase font-bold">Portfolio & Web</p>
                                            <div className="flex flex-col gap-1">
                                                {req.kind.NewCreator.portfolio[0] && (
                                                    <a href={req.kind.NewCreator.portfolio[0]} target="_blank" className="text-xs text-inda-blue hover:underline flex items-center gap-1">
                                                        Portfolio <ExternalLink className="w-3 h-3" />
                                                    </a>
                                                )}
                                                {req.kind.NewCreator.webSite[0] && (
                                                    <a href={req.kind.NewCreator.webSite[0]} target="_blank" className="text-xs text-inda-blue hover:underline flex items-center gap-1">
                                                        Official Site <ExternalLink className="w-3 h-3" />
                                                    </a>
                                                )}
                                            </div>
                                        </div>

                                        <div className="space-y-1">
                                            <p className="text-[10px] text-zinc-500 uppercase font-bold">Government ID</p>
                                            <div className="text-xs text-zinc-300 font-mono">
                                                {Object.entries(req.kind.NewCreator.governmentID[0] || {}).map(([type, val]: any) => (
                                                    <div key={type} className="flex gap-2">
                                                        <span className="text-zinc-500 uppercase">{type}:</span>
                                                        <span>{typeof val === 'bigint' ? val.toString() : val}</span>
                                                    </div>
                                                ))}
                                            </div>
                                        </div>
                                    </>
                                )}

                                {/* Metadata Parser */}
                                {req.metadata.length > 0 && (
                                    <div className="col-span-full pt-4 border-t border-white/5">
                                        <p className="text-[10px] text-zinc-500 uppercase font-bold mb-2">Additional Metadata</p>
                                        <div className="flex flex-wrap gap-2">
                                            {req.metadata.map((m: any, i: number) => (
                                                <div key={i} className="text-[10px] bg-white/5 px-2 py-1 rounded border border-white/5">
                                                    <span className="text-inda-purple font-bold">{m.key}:</span> {renderMetadataValue(m.value)}
                                                </div>
                                            ))}
                                        </div>
                                    </div>
                                )}
                            </div>

                            {/* Acciones */}
                            <div className="flex lg:flex-col justify-center gap-3 border-l border-white/5 pl-6">
                                <button className="p-3 bg-emerald-500/10 text-emerald-500 border border-emerald-500/20 rounded-xl hover:bg-emerald-500 hover:text-white transition-all flex items-center gap-2 text-xs font-bold">
                                    <ShieldCheck className="w-4 h-4" /> Approve
                                </button>
                                <button className="p-3 bg-red-500/10 text-red-500 border border-red-500/20 rounded-xl hover:bg-red-500 hover:text-white transition-all flex items-center gap-2 text-xs font-bold">
                                    <XCircle className="w-4 h-4" /> Reject
                                </button>
                            </div>

                        </div>
                    </div>
                ))}

                {requests.length === 0 && (
                    <div className="text-center py-20 bg-zinc-900/20 border border-dashed border-white/10 rounded-3xl">
                        <p className="text-zinc-500 italic">No pending requests at the moment.</p>
                    </div>
                )}
            </div>
            {selectedUser && (
                <UserModal user={selectedUser} onClose={() => setSelectedUser(null)} />
            )}

            {isFetchingUser && (
                <div className="fixed inset-0 z-50 flex items-center justify-center bg-black/20 backdrop-blur-[2px]">
                    <div className="bg-zinc-900 px-6 py-3 rounded-full border border-white/10 text-white font-bold animate-bounce">
                        Loading Sovereing Identity...
                    </div>
                </div>
            )}
        </div>
    );
};

export default AdminPage;