export const COUNTRY_IDENTITY_CONFIG = {
  AR: {
    name: "Argentina", flag: "🇦🇷",
    documents: [
      { id: "DNI", label: "DNI", placeholder: "Ej: 40123456", isNumeric: true, maxDigits: 8 },
      { id: "CUIL", label: "CUIL", placeholder: "11 dígitos", isNumeric: true, maxDigits: 11 }
    ]
  },
  MX: {
    name: "México", flag: "🇲🇽",
    documents: [
      { id: "CURP", label: "CURP", placeholder: "18 caracteres", isNumeric: false, maxDigits: 18 },
      { id: "RFC", label: "RFC", placeholder: "13 caracteres", isNumeric: false, maxDigits: 13 }
    ]
  },
  CO: {
    name: "Colombia", flag: "🇨🇴",
    documents: [
      { id: "CC", label: "Cédula de Ciudadanía", placeholder: "Hasta 10 dígitos", isNumeric: true, maxDigits: 10 },
      { id: "NIT", label: "NIT", placeholder: "Número tributario", isNumeric: true, maxDigits: 10 }
    ]
  },
  CL: {
    name: "Chile", flag: "🇨🇱",
    documents: [
      { id: "RUN", label: "RUN / RUT", placeholder: "Sin puntos ni guion (ej: 12345678K)", isNumeric: false, maxDigits: 9 }
    ]
  },
  PE: {
    name: "Perú", flag: "🇵🇪",
    documents: [
      { id: "DNI", label: "DNI", placeholder: "8 dígitos", isNumeric: true, maxDigits: 8 },
      { id: "RUC", label: "RUC", placeholder: "11 dígitos", isNumeric: true, maxDigits: 11 }
    ]
  },
  ES: {
    name: "España", flag: "🇪🇸",
    documents: [
      { id: "DNI", label: "DNI", placeholder: "8 números + letra", isNumeric: false, maxDigits: 9 },
      { id: "NIE", label: "NIE (Extranjeros)", placeholder: "Letra + 7 núm + letra", isNumeric: false, maxDigits: 9 }
    ]
  },
  US: {
    name: "United States", flag: "🇺🇸",
    documents: [
      { id: "SSN", label: "Social Security Number", placeholder: "9 digits only", isNumeric: true, maxDigits: 9 },
      { id: "PASSPORT", label: "Passport", placeholder: "Alphanumeric ID", isNumeric: false, maxDigits: 20 }
    ]
  },
  VE: {
    name: "Venezuela", flag: "🇻🇪",
    documents: [
      { id: "CI", label: "Cédula de Identidad", placeholder: "Sólo números", isNumeric: true, maxDigits: 9 },
      { id: "RIF", label: "RIF", placeholder: "Ej: V123456789", isNumeric: false, maxDigits: 10 }
    ]
  },
  UY: {
    name: "Uruguay", flag: "🇺🇾",
    documents: [
      { id: "CI", label: "Cédula de Identidad", placeholder: "8 dígitos", isNumeric: true, maxDigits: 8 }
    ]
  },
  IR: {
    name: "Iran", flag: "🇮🇷",
    documents: [
      { 
        id: "MELLI_CODE", label: "National ID (Melli Code)", placeholder: "10 digits (e.g. 0012345678)", isNumeric: true, maxDigits: 10 
      },
      { 
        id: "PASSPORT", label: "Passport", placeholder: "9 characters (e.g. A12345678)", isNumeric: false, maxDigits: 9 
      }
    ]
  },
  EC: {
    name: "Ecuador", flag: "🇪🇨",
    documents: [
      { id: "CI", label: "Cédula de Identidad", placeholder: "10 dígitos", isNumeric: true, maxDigits: 10 },
      { id: "RUC", label: "RUC", placeholder: "13 dígitos", isNumeric: true, maxDigits: 13 }
    ]
  },
  BO: {
    name: "Bolivia", flag: "🇧🇴",
    documents: [
      { id: "CI", label: "Cédula de Identidad", placeholder: "7 a 9 dígitos + Extensión", isNumeric: false, maxDigits: 12 },
      { id: "NIT", label: "NIT", placeholder: "Número de Identificación Tributaria", isNumeric: true, maxDigits: 15 }
    ]
  },
  BR: {
    name: "Brazil", flag: "🇧🇷",
    documents: [
      { id: "CPF", label: "CPF", placeholder: "11 dígitos (solo números)", isNumeric: true, maxDigits: 11 },
      { id: "RG", label: "RG", placeholder: "Registro Geral", isNumeric: false, maxDigits: 12 }
    ]
  },
  SV: {
    name: "El Salvador", flag: "🇸🇻",
    documents: [
      { id: "DUI", label: "DUI", placeholder: "9 dígitos (sin guion)", isNumeric: true, maxDigits: 9 },
      { id: "NIT", label: "NIT", placeholder: "14 dígitos", isNumeric: true, maxDigits: 14 }
    ]
  },
  PR: {
    name: "Puerto Rico", flag: "🇵🇷",
    documents: [
      { id: "SSN", label: "Social Security Number", placeholder: "9 digits", isNumeric: true, maxDigits: 9 },
      { id: "DRIVERS_LICENSE", label: "Licencia de Conducir", placeholder: "Alfanumérico", isNumeric: false, maxDigits: 15 }
    ]
  },
  OTHER: {
    name: "Other / International", flag: "🌐",
    documents: [
      { id: "PASSPORT", label: "Passport", placeholder: "ID Number", isNumeric: false, maxDigits: 25 },
      { id: "ID", label: "National ID", placeholder: "Identification value", isNumeric: false, maxDigits: 25 }
    ]
  }
};