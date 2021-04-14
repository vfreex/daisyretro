pub mod ines;
pub mod mapper;
pub mod cartridge;

const PRG_BANK_SIZE : usize = 16 * 1024; // bytes in a PRG/ROM bank
const CHR_BANK_SIZE : usize= 8 * 1024;  // bytes in a CHR/VROM bank
