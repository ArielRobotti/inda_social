import { useNavigate } from "react-router";
import {
  ConnectWalletDropdownMenu,
  ConnectWalletDropdownMenuItems,
  ConnectWalletDropdownMenuDisconnectItem,
  ConnectWalletDropdownMenuButton,
  ConnectWalletDropdownMenuItem,
} from "@nfid/identitykit/react";
import { useSession } from "../context/SessionContext";
import type { User, Creator, Brand, Partnership } from "../declarations/backend/backend.did";
import { ShieldCheck, Home } from "lucide-react";


const UserMenu = ({ user, role, onClose }: { user: User, role: Creator | Brand | Partnership | null, onClose: () => void }) => {
  const { logout, isAdmin } = useSession();
  const navigate = useNavigate();

  const closeAllMenus = (closeIdentityMenu: () => void) => {
    // 1. Cerramos el menú lateral de Navbar (si estamos en móvil)
    if (onClose) onClose();

    // 2. Cerramos el Dropdown de IdentityKit usando la función close de Headless UI
    if (closeIdentityMenu) closeIdentityMenu();

    // 3. Forzamos un pequeño blur para asegurar que los elementos enfocados se cierren
    if (document.activeElement instanceof HTMLElement) {
      document.activeElement.blur();
    }
  };

  const handleNavigation = (path: string, closeIdentityMenu: () => void) => {
    closeAllMenus(closeIdentityMenu);
    // Navegamos inmediatamente
    navigate(path);
  };

  const initials = user.firstName[0].toUpperCase() + " " + user.lastName[0].toUpperCase();
  console.log(role)


  return (
    <div className="flex items-center gap-3 ">

      {/* Menú Desplegable de IdentityKit con render prop para acceder a 'close' */}
      <ConnectWalletDropdownMenu>
        {({ close }: { close: () => void }) => (
          <>
            {/* Este es el activador (el avatar) */}
            <ConnectWalletDropdownMenuButton className="p-0 bg-transparent hover:bg-transparent border-none outline-none focus:outline-none focus:ring-0 focus-visible:outline-none focus-visible:ring-0 shadow-none">
              <div className="group relative ">
                {/* Efecto de resplandor de fondo (Glow) */}
                <div className="absolute -inset-0.5 bg-linear-to-r from-inda-blue to-inda-purple rounded-full opacity-30 group-hover:opacity-60 blur-md transition-opacity duration-300"></div>

                {/* Círculo Principal */}
                <div className="relative w-12 h-12 rounded-full bg-zinc-800 flex items-center justify-center cursor-pointer hover:scale-105 transition-all duration-300 border border-white/10 overflow-hidden">

                  {/* Gradiente interno sutil */}
                  <div className="absolute inset-0 bg-linear-to-br from-inda-blue/20 via-transparent to-inda-purple/20"></div>

                  {/* Iniciales con tipografía técnica */}
                  <span className="relative z-10 text-transparent bg-clip-text bg-linear-to-r from-white to-zinc-400 font-black text-lg tracking-tighter">
                    {initials}
                  </span>
                </div>
              </div>
            </ConnectWalletDropdownMenuButton>


            {/* Los items del menú */}
            <ConnectWalletDropdownMenuItems className="bg-zinc-900 border border-zinc-800 shadow-2xl rounded-xl p-2 mt-2 w-100 select-none">

              <ConnectWalletDropdownMenuItem
                className="p-4 border-b border-zinc-800 mb-2 flex items-start gap-1 cursor-pointer transition-colors hover:bg-white/5"
                onClick={() => handleNavigation("/", close)}
              >
                <span className="text-lg text-[16px] font-bold text-white truncate w-60 hover:text-blue-300">Home</span>
                <Home />

              </ConnectWalletDropdownMenuItem>

              <ConnectWalletDropdownMenuItem
                className="p-4 border-b border-zinc-800 mb-2 flex items-start gap-1 cursor-pointer transition-colors hover:bg-white/5"
                onClick={() => handleNavigation("/profileSettings", close)}
              >
                <span className="text-lg text-[16px] font-bold text-white truncate w-60 hover:text-blue-300">{user.firstName} {user.lastName}</span>
                {/* Badge de estado si no tiene rol */}
                {!role && !user.roleRequestedOrAsigned && (
                  <span className="text-[10px] bg-amber-500/10 text-amber-500 px-2 py-0.5 rounded-full border border-amber-500/20 mt-1">
                    Setup Account Role
                  </span>
                )}

              </ConnectWalletDropdownMenuItem>
              {isAdmin && (
                <ConnectWalletDropdownMenuItem
                  onClick={() => handleNavigation("/adminPannel", close)}
                  className="w-full flex items-center gap-2 p-3 text-inda-400 hover:bg-blue-500/10 rounded-lg transition-colors cursor-pointer font-medium border-none outline-none"
                >
                  Admin Panel
                  <ShieldCheck className="w-4 h-4" />
                </ConnectWalletDropdownMenuItem>
              )}

              <ConnectWalletDropdownMenuDisconnectItem
                onClick={async () => {
                  closeAllMenus(close);
                  navigate("/")
                  logout();
                }}
                className="w-full flex items-center gap-2 p-3 text-red-400 hover:bg-red-500/10 rounded-lg transition-colors cursor-pointer font-medium"
              >
                Disconnect
              </ConnectWalletDropdownMenuDisconnectItem>
            </ConnectWalletDropdownMenuItems>
          </>
        )}
      </ConnectWalletDropdownMenu>
    </div>
  );
};

export default UserMenu;