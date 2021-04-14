//! The .NES file format (file name suffix .nes) is the de facto standard for distribution of NES binary programs.
//!
//! See http://fms.komkon.org/EMUL8/NES.html#LABM and https://wiki.nesdev.com/w/index.php/INES#iNES_file_format

use std::error::Error;
use std::fs::File;
use std::io::Read;
use std::mem::transmute;

/// String "NES^Z" used to recognize .NES files.
const INES_MAGIC_BYTES: &[u8; 4] = b"NES\x1a";
const PRG_ROM_BYTES_PER_BANK: usize = 16 * 1024;
const CHR_ROM_BYTES_PER_BANK: usize = 8 * 1024;
const RAM_BYTES_PER_BANK: usize = 8 * 1024;
const TRAINER_BYTES: usize = 512;

const FLAGS6_TRAINER_PRESENT: u8 = 1 << 2;

#[derive(Debug)]
#[repr(u8)]
enum Flags6 {
    VERTICAL_MIRRORING = 1,
    BATTERY_BACKED_RAM_PRESENT = 1 << 1,
    TRAINER_PRESENT = 1 << 2,
    FOUR_SCREEN_VRAM_PRESENT = 1 << 3,
}

#[derive(Debug)]
#[repr(u8)]
enum Flags7 {
    VS_SYSTEM = 1,
}

#[derive(Debug)]
#[repr(u8)]
enum Flags9 {
    TV_SYSTEM_PAL = 1,
}

/// iNES file header
#[derive(Debug, Default)]
#[repr(C)]
pub struct InesHeader {
    /// String "NES^Z" used to recognize .NES files
    pub magic: [u8; 4],
    /// Number of 16kiB PRG ROM banks
    pub prg_rom_banks: u8,
    /// Number of 8kiB CHR ROM banks
    pub chr_rom_banks: u8,
    pub flags6: u8,
    pub flags7: u8,
    /// Number of 8kiB RAM banks. Value 0 infers 8 KB for compatibility
    pub ram_banks: u8,
    pub flags9: u8,
    pub unused: [u8; 6],
}

impl InesHeader {
    pub fn parse(bytes: &[u8; 16]) -> Result<InesHeader, Box<dyn Error>> {
        let header: InesHeader = unsafe { transmute(*bytes) };
        if &header.magic != INES_MAGIC_BYTES {
            return Err("Couldn't parse iNES header: bytes doesn't include the iNES magic bytes.".into());
        }
        Ok(header)
    }

    pub fn has_trainer(&self) -> bool {
        self.flags6 & Flags6::TRAINER_PRESENT as u8 != 0
    }

    pub fn get_mapper_type(&self) -> u8 {
        // Older versions of the iNES emulator ignored bytes 7-15, and several ROM management tools wrote messages in there.
        // if the last 4 bytes are not all zero, and the header is not marked for NES 2.0 format,
        // an emulator should either mask off the upper 4 bits of the mapper number or simply refuse to load the ROM.
        if self.unused[2..] != [0u8; 4] {
            return self.flags6 >> 4
        }
        (self.flags7 & 0xf0) | (self.flags6 >> 4)
    }
}

/// iNES ROM
#[derive(Debug, Default)]
#[repr(C)]
pub struct InesRom {
    pub header: InesHeader,
    pub trainer: Vec<u8>,
    pub prg_rom: Vec<u8>,
    pub chr_rom: Vec<u8>,
    pub extra_bytes: Vec<u8>,
}

impl InesRom {
    pub fn from_file(path: &str) -> Result<InesRom, Box<dyn Error>> {
        let mut file = File::open(path)?;
        let mut buf = [0u8; 16];
        file.read_exact(&mut buf)?;

        let header: InesHeader = InesHeader::parse(&buf)?;
        let mut rom = InesRom {
            trainer: vec![0u8; header.has_trainer().then(|| TRAINER_BYTES).unwrap_or(0)],
            prg_rom: vec![0u8; header.prg_rom_banks as usize * PRG_ROM_BYTES_PER_BANK],
            chr_rom: vec![0u8; header.chr_rom_banks as usize * CHR_ROM_BYTES_PER_BANK],
            extra_bytes: vec![],
            header,
        };

        file.read_exact(&mut rom.trainer)?;
        file.read_exact(&mut rom.prg_rom)?;
        file.read_exact(&mut rom.chr_rom)?;
        file.read_to_end(&mut rom.extra_bytes)?;

        if rom.header.get_mapper_type() != 0 {
            return Err(format!("Unsupported mapper type {}", rom.header.get_mapper_type()).into());
        }

        Ok(rom)
    }
}
