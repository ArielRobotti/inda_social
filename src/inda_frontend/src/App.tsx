
import { Toaster } from "@/components/ui/toaster";
import { Toaster as Sonner } from "@/components/ui/sonner";
import { TooltipProvider } from "./components/ui/tooltip";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { Routes, Route, BrowserRouter } from "react-router";
import Index from "./pages/Index";
import ProfileSettings from "./pages/profileSettings";
import AdminPage from "./pages/adminPage"
import NotFound from "./pages/NotFound";
import Navbar from "./components/Navbar";

const queryClient = new QueryClient();

const App = () => (
  <BrowserRouter>
    <QueryClientProvider client={queryClient}>
      <TooltipProvider>
        <Toaster />
        <Sonner />
        <Navbar />
          <Routes>
            <Route path="/" element={<Index />} />
            <Route path="/profileSettings" element={<ProfileSettings />}/>
            <Route path="/adminPannel" element={<AdminPage/>}/>
            <Route path="*" element={<NotFound />} />
          </Routes>
      </TooltipProvider>
    </QueryClientProvider>
  </BrowserRouter>
);

export default App;
