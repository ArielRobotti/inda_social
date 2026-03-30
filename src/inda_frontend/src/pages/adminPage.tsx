/* eslint-disable @typescript-eslint/no-explicit-any */
import React, { useEffect, useState } from 'react';
import { ShieldCheck, XCircle, ExternalLink, User as UserIcon, FileText } from 'lucide-react';
import { useSession } from '@/context/SessionContext';
import { Request } from '@/declarations/backend/backend.did';
// import Button from '../components/Button';
import { toast } from 'sonner';
import { Principal } from '@dfinity/principal';

const AdminPage = () => {
    const [requests, setRequests] = useState<[Principal, Request][]>([]);
    const [loading, setLoading] = useState(true);
    const { backend } = useSession();

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
        <div className="max-w-7xl mx-auto pt-24 px-6 pb-20">
            <header className="mb-10 flex justify-between items-end">
                <div>
                    <h1 className="text-3xl font-black text-white uppercase tracking-tighter">Pending Authorizations</h1>
                    <p className="text-zinc-500 text-sm">Review and verify decentralized identity requests.</p>
                </div>
                <div className="bg-zinc-900 border border-white/5 px-4 py-2 rounded-full text-[10px] font-mono text-inda-blue">
                    {requests.length} PENDING REQUESTS
                </div>
            </header>

            <div className="space-y-4">
                {requests.map(([principal, req]) => (
                    <div key={principal.toString()} className="bg-zinc-900/40 border border-white/5 rounded-3xl p-6 backdrop-blur-md hover:border-white/10 transition-all">
                        <div className="flex flex-col lg:flex-row gap-8">

                            {/* Info Principal */}
                            <div className="flex-1 space-y-4">
                                <div className="flex items-center gap-3">
                                    <div className="w-10 h-10 rounded-full bg-linear-to-br from-inda-blue to-inda-purple flex items-center justify-center">
                                        <UserIcon className="w-5 h-5 text-white" />
                                    </div>
                                    <div>
                                        <p className="text-[10px] text-zinc-500 font-mono uppercase tracking-widest">Principal ID</p>
                                        {/* <p className="text-sm font-bold text-white truncate w-48 lg:w-full">{req[1].kind}</p> */}
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
                                                {req[1].kind.NewCreator.webSite[0] && (
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
        </div>
    );
};

export default AdminPage;