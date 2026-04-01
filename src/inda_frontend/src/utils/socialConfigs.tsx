export const SOCIAL_CONFIG = {
  // Redes Tradicionales
  ig: { label: 'Instagram', icon: '📸', pattern: /(?:instagr\.am|instagram\.com)\/([a-zA-Z0-9.]+)/ },
  tw: { label: 'X / Twitter', icon: '🐦', pattern: /(?:twitter\.com|x\.com)\/([a-zA-Z0-9_]+)/ },
  fb: { label: 'Facebook', icon: '🌐', pattern: /facebook\.com\/(?:profile\.php\?id=\d+|([\w.]+))/ },
  yt: { label: 'YouTube', icon: '📺', pattern: /(?:youtube\.com\/(?:@|user\/|c\/)|youtu\.be\/)([a-zA-Z0-9_-]+)/ },
  li: { label: 'LinkedIn', icon: '💼', pattern: /linkedin\.com\/in\/([a-zA-Z0-9_-]+)/ },
  tk: { label: 'TikTok', icon: '🎵', pattern: /tiktok\.com\/@([a-zA-Z0-9.]+)/ },
  
  // Plataformas de Creadores / 3D (Clave para Indasocial)
  as: { label: 'ArtStation', icon: '🎨', pattern: /artstation\.com\/([a-zA-Z0-9_-]+)/ },
  sf: { label: 'Sketchfab', icon: '📦', pattern: /sketchfab\.com\/([a-zA-Z0-9_-]+)/ },
  be: { label: 'Behance', icon: '🅱️', pattern: /behance\.net\/([a-zA-Z0-9_-]+)/ },
  gh: { label: 'GitHub', icon: '💻', pattern: /github\.com\/([a-zA-Z0-9_-]+)/ },
};