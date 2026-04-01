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
  // ES: {
  //   name: "España",
  //   flag: "🇪🇸",
  //   documents: [
  //     { id: "DNI", label: "DNI", placeholder: "8 números + letra", pattern: /^\d{8}[A-Z]$/i },
  //     { id: "NIE", label: "NIE (Extranjeros)", placeholder: "Letra + 7 números + letra", pattern: /^[XYZ]\d{7}[A-Z]$/i }
  //   ]
  // },
  // US: {
  //   name: "United States",
  //   flag: "🇺🇸",
  //   documents: [
  //     { id: "SSN", label: "Social Security Number", placeholder: "9 digits", pattern: /^\d{9}$/ },
  //     { id: "PASSPORT", label: "Passport", placeholder: "Alphanumeric", pattern: /^[A-Z0-9]+$/i }
  //   ]
  // },
  // OTHER: {
  //   name: "Other / International",
  //   flag: "🌐",
  //   documents: [
  //     { id: "PASSPORT", label: "Passport", placeholder: "Passport ID", pattern: /.+/ },
  //     { id: "OTHER", label: "Other ID", placeholder: "Enter ID value", pattern: /.+/ }
  //   ]
  // }
};