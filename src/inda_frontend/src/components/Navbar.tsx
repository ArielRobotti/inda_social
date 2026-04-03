import { useEffect, useState } from "react";
import { ConnectWallet } from "@nfid/identitykit/react";
import Logo from './Logo';
import { useSession } from "../context/SessionContext";
import Button from './Button';
import UserMenu from "./UserMenu";
import { cn } from '@/lib/utils';
/* Asegúrate de importar estos iconos de Lucide */
import { Menu, X } from "lucide-react";
import RegistrationModal from "./RegistrationModal";

// eslint-disable-next-line @typescript-eslint/no-explicit-any
const MyCustomButton = (props: any) => {
  const { loading, onClick, ...restProps } = props;
  
  const handleClick = (e: React.MouseEvent) => {
    // Si hay un onClick original de IdentityKit, lo llamamos
    if (onClick) onClick(e);
    // Y si existe una forma de cerrar el menú global (si estamos en móvil), lo hacemos
    // Pero aquí no tenemos acceso directo a setIsMenuOpen a menos que lo pasemos o usemos un evento
  };

  return (
    <Button
      className="text-white py-1 w-full md:w-40 transition-colors duration-200"
      onClick={onClick}
      {...restProps}
    >
      {loading ? "Connecting..." : "Connect"}
    </Button>
  );
};

const links = [
  { name: 'Candid UI', href: 'https://a4gq6-oaaaa-aaaab-qaa4q-cai.raw.icp0.io/?id=st35g-iaaaa-aaaal-ascpq-cai' },
  { name: 'Features', href: '/#features' },
  { name: 'Token', href: '/#token' },
  { name: 'Roadmap', href: '/#roadmap' },
  { name: 'Community', href: '/#community' },
  { name: 'Blog', href: '/blog'} 
];

const Navbar = () => {
  const { user, loading, role, principalID } = useSession();
  const [isScrolled, setIsScrolled] = useState(false);
  const [isMenuOpen, setIsMenuOpen] = useState(false);

  useEffect(() => {
    const handleScroll = () => {
      setIsScrolled(window.scrollY > 20);
    };
    window.addEventListener('scroll', handleScroll);
    return () => window.removeEventListener('scroll', handleScroll);
  }, []);


  return (
    <nav className={cn(
      'fixed top-0 left-0 right-0 z-50 transition-all duration-300',
      isScrolled ? 'bg-white/80 backdrop-blur-md shadow-sm' : 'bg-transparent'
    )}>
      <div className="container mx-auto px-4">
        <div className="flex items-center justify-between h-16">
          <a href="/" onClick={() => setIsMenuOpen(false)} className="flex items-center">
            <Logo variant={isScrolled ? 'default' : 'white'} />
          </a>

          {/* Desktop menu links */}
          <div className="hidden md:flex items-center space-x-1">
            {links.map(link => (
              <a
                key={link.name}
                href={link.href}
                className={cn(
                  'px-3 py-2 rounded-md text-sm font-medium transition-colors',
                  isScrolled ? 'text-gray-700 hover:text-inda-blue' : 'text-white/80 hover:text-white'
                )}
              >
                {link.name}
              </a>
            ))}
          </div>

          {/* Wallet / User Section + Mobile Toggle */}
          <div className="flex items-center gap-4">
            <div className="hidden md:block">
              {loading && !user ? (
                <div className="h-8 w-24 animate-pulse bg-zinc-800 rounded-full" />
              ) : user ? (
                <UserMenu user={user} role={role} onClose={() => setIsMenuOpen(false)}/>
              ) : (
                <ConnectWallet connectButtonComponent={MyCustomButton} />
              )}
            </div>

            {/* BOTÓN HAMBURGUESA (Lo que faltaba) */}
            <div className="md:hidden">
              <button
                onClick={() => setIsMenuOpen(!isMenuOpen)}
                className={cn(
                  'inline-flex items-center justify-center p-2 rounded-md transition-colors',
                  isScrolled ? 'text-gray-700' : 'text-white'
                )}
              >
                {isMenuOpen ? <X className="h-6 w-6" /> : <Menu className="h-6 w-6" />}
              </button>
            </div>
          </div>
        </div>
      </div>

      {/* Menú móvil con slide y visibilidad controlada */}
      <div
        className={cn(
          /* Base: Fijo, a la derecha, con z-index alto */
          'fixed top-16 z-50 bg-white shadow-2xl transition-all duration-300 ease-in-out md:hidden',
          /* Tamaño */
          'w-full h-auto max-h-[400px]  border-gray-100',
          /* ANIMACIÓN CRÍTICA: */
          isMenuOpen
            ? 'translate-x-0 opacity-100 visible'
            : 'translate-x-full opacity-0 invisible'
        )}
      >
        <div className="flex flex-col p-6 space-y-4">
          <div className="pt-4 border-t border-gray-100">
            {user && (
              <UserMenu user={user} role={role} onClose={() => setIsMenuOpen(false)}/>
            )}
          </div>
          {links.map(link => (
            <a
              key={link.name}
              href={link.href}
              className="text-lg font-semibold text-gray-700 hover:text-inda-blue transition-colors"
              onClick={() => setIsMenuOpen(false)}
            >
              {link.name}
            </a>
          ))}

          <div className="text-center pt-4 border-t border-gray-100">
            {!user && (
              <div onClick={() => setIsMenuOpen(false)}>
                <ConnectWallet connectButtonComponent={MyCustomButton} />
              </div>
            )}
          </div>

        </div>
      </div>

      {/* Overlay: Esto es lo que evita que se vea "siempre" el fondo si hay bugs de renderizado */}
      {isMenuOpen && (
        <div
          className="fixed inset-0 bg-black/40 backdrop-blur-sm z-40 md:hidden"
          onClick={() => setIsMenuOpen(false)}
        />
      )}
      {/* Modal registro de usuario base (visitante) */}
      {!user && !loading && principalID != "" && (
        <RegistrationModal />
      )}
    </nav>
  );
};

export default Navbar;