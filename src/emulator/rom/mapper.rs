use crate::emulator::rom::cartridge::Cartridge;
use std::error::Error;
use std::fmt::Debug;
use crate::emulator::rom::{PRG_BANK_SIZE, CHR_BANK_SIZE};

type ReadResult = Result<u8, Box<dyn Error>>;
type WriteResult = Result<(), Box<dyn Error>>;
type R = Fn(u16) -> ReadResult;
type W = Fn(u16, u8) -> WriteResult;

pub trait Mapper: Debug {
    fn cpu_read(&self, addr: u16, on_read: &dyn Fn(usize) -> ReadResult) -> ReadResult;
    fn cpu_write(&mut self, addr: u16, val: u8, on_write: &dyn Fn(usize, u8) -> WriteResult, on_mirroring_change: &mut dyn FnMut([usize; 4]) -> WriteResult) -> WriteResult;
    fn ppu_read(&self, addr: u16, on_read: &dyn Fn(usize) -> ReadResult) -> ReadResult;
    fn ppu_write(&mut self, addr: u16, val: u8, on_write: &dyn Fn(usize, u8) -> WriteResult) -> WriteResult;
}

#[derive(Debug)]
pub struct NROMMapper {
    prg_rom_size: usize,
    chr_rom_size: usize,
    prg_ram_size: usize,
    prg_ram: Vec<u8>,
}

impl NROMMapper {
    pub fn new(prg_rom_size: usize, chr_rom_size: usize, prg_ram_size: usize) -> Self {
        NROMMapper {
            prg_rom_size,
            chr_rom_size,
            prg_ram_size,
            prg_ram: vec![0; prg_ram_size as usize],
        }
    }
}

impl Mapper for NROMMapper {
    fn cpu_read(&self, addr: u16, on_read: &dyn Fn(usize) -> ReadResult) -> ReadResult {
        match addr {
            0x6000..=0x7fff => Ok(self.prg_ram[(addr - 0x6000) as usize]),
            0x8000..=0xbfff => {
                on_read(addr as usize - 0x8000)
            }
            0xc000..=0xffff => {
                let phy = addr as usize - 0x8000;
                if phy > self.prg_rom_size {
                    on_read(phy - 0x4000)
                } else {
                    on_read(phy)
                }
            }
            _ => Err(format!("Mapper #0 - Cannot read CPU memory address {:04x}: unmapped address", addr).into())
        }
    }

    fn cpu_write(&mut self, addr: u16, val: u8, on_write: &dyn Fn(usize, u8) -> WriteResult, on_mirroring_change: &mut dyn FnMut([usize; 4]) -> WriteResult) -> WriteResult {
        match addr {
            0x6000..=0x7fff => {
                self.prg_ram[(addr - 0x6000) as usize] = val;
                Ok(())
            }
            0x8000..=0xffff => {
                Err(format!("Mapper #0 - Cannot write to cpu memory address {:04x}: read-only memory", addr).into())
            }
            _ => Err(format!("Mapper #0 - Cannot write to cpu memory address {:04x}: unmapped address", addr).into())
        }
    }

    fn ppu_read(&self, addr: u16, on_read: &dyn Fn(usize) -> ReadResult) -> ReadResult {
        match addr {
            0x0000..=0x2000 => {
                on_read(addr as usize)
            }
            _ => Err(format!("Mapper #0 - Cannot read PPU memory address {:04x}: unmapped address", addr).into())
        }
    }

    fn ppu_write(&mut self, addr: u16, val: u8, on_write: &dyn Fn(usize, u8) -> WriteResult) -> WriteResult {
        Err("Mapper #0: Couldn't write to CHR ROM".into())
    }
}

/// https://wiki.nesdev.com/w/index.php/MMC1
#[derive(Debug)]
pub struct Mmc1Mapper {
    prg_ram: [u8; 0x2000],
    chr_ram: [u8; CHR_BANK_SIZE],
    shift_register: u8,
    registers: [u8; 4],
    write_count: u8,
    prg_rom_size: usize,
    chr_rom_size: usize,
    prg_ram_size: usize,
}

impl Mmc1Mapper {
    pub fn new(prg_rom_size: usize, chr_rom_size: usize, prg_ram_size: usize) -> Self {
        Self { prg_ram: [0; 0x2000], chr_ram: [0; CHR_BANK_SIZE], shift_register: 0, registers: [0x0c, 0, 0, 0], write_count: 0, prg_rom_size, chr_rom_size, prg_ram_size }
    }

    fn map_prg_address(&self, addr: u16) -> Result<usize, Box<dyn Error>> {
        let offset = addr & 0x3fff;
        let mut bank = (self.registers[3] & 0x0f) as usize;
        match self.registers[0] >> 2 & 3 {
            0 | 1 => {
                // Switchable 32K Area at 8000h-FFFFh
                bank &= 0x0e;
                if addr >= 0xc000 {
                    bank += 1;
                }
            }
            2 => {
                // Switchable 16K Area at C000h-FFFFh (via Register 3)
                // And Fixed  16K Area at 8000h-BFFFh (always 1st 16K)
                if addr < 0xc000 {
                    bank = 0
                }
            }
            3 => {
                // Switchable 16K Area at 8000h-BFFFh (via Register 3)
                // And Fixed  16K Area at C000h-FFFFh (always last 16K)
                if addr >= 0xc000 {
                    bank = self.prg_rom_size / PRG_BANK_SIZE - 1;
                }
            }
            _ => panic!()
        }
        let phy_addr = bank * PRG_BANK_SIZE | offset as usize;
        if phy_addr >= self.prg_rom_size {
            return Err(format!("MMC1: Error mapping address {:04x}", addr).into())
        }
        Ok(phy_addr)
    }

    fn map_chr_address(&self, addr: u16) -> Result<usize, Box<dyn Error>> {
        let offset = addr & 0x3fff;
        let mut bank;
        if self.registers[0] & 0x10 != 0 {
            // Swap 4K of VROM at PPU 0000h and 1000h
            if addr < 0x1000 {
                bank = (self.registers[1] & 0x1f) as usize;
            } else {
                bank = (self.registers[2] & 0x1f) as usize;
            }
        } else {
            // Swap 8K of VROM at PPU 0000h
            bank = (self.registers[1] & 0x1e) as usize;
            if addr >= 0x1000 {
                bank += 1;
            }
        }

        let phy_addr = bank * CHR_BANK_SIZE / 2 | offset as usize;
        if self.chr_rom_size > 0 && phy_addr >= self.chr_rom_size || self.chr_rom_size == 0 && phy_addr >= self.chr_ram.len() {
            return Err(format!("MMC1: Error mapping VRAM address ${:04x}", addr).into())
        }
        Ok(phy_addr)
    }
}

impl Mapper for Mmc1Mapper {
    fn cpu_read(&self, addr: u16, on_read: &dyn Fn(usize) -> ReadResult) -> ReadResult {
        let phy_addr = self.map_prg_address(addr)?;
        on_read(phy_addr)
    }

    fn cpu_write(&mut self, addr: u16, val: u8, on_write: &dyn Fn(usize, u8) -> WriteResult, on_mirroring_change: &mut dyn FnMut([usize; 4]) -> WriteResult) -> WriteResult {
        match addr {
            0x6000..=0x7fff => {
                self.prg_ram[addr as usize - 0x6000] = val;
                Ok(())
            }
            0x8000..=0xffff => {  // $8000-$FFFF is connected to a common shift register
                if val & 0x80 != 0 {
                    // Writing a value with bit 7 set ($80 through $FF) clears the shift register to its initial state
                    self.shift_register = 0;
                    self.write_count = 0;
                } else {
                    //  On the first four writes, the MMC1 shifts bit 0 into a shift register
                    self.write_count += 1;
                    self.shift_register >>= 1;
                    self.shift_register |= (val & 1) << 4;
                    if self.write_count == 5 {
                        // On the fifth write, the MMC1 copies bit 0 and the shift register contents
                        // into an internal register selected by bits 14 and 13 of the address,
                        // and then it clears the shift register.
                        let idx = (addr >> 13 & 3) as usize;
                        self.registers[idx] = self.shift_register;
                        self.shift_register = 0;
                        self.write_count = 0;
                        if idx == 0 { // the control register is changed
                            match self.registers[idx] & 3 {
                                0 => { // one-screen, lower bank
                                    on_mirroring_change([0, 0, 0, 0]);
                                }
                                1 => { // one-screen, upper bank
                                    on_mirroring_change([1, 1, 1, 1]);
                                }
                                2 => { // vertical
                                    on_mirroring_change([0, 1, 0, 1]);
                                }
                                3 => { // horizontal
                                    on_mirroring_change([0, 0, 1, 1]);
                                }
                                _ => {}
                            }
                        }
                    }
                }
                Ok(())
            }
            _ => Err(format!("MMC1 Mapper: Address {:04x} is not writable", addr).into())
        }
    }

    fn ppu_read(&self, addr: u16, on_read: &dyn Fn(usize) -> ReadResult) -> ReadResult {
        let phy_addr = self.map_chr_address(addr)?;
        if self.chr_rom_size > 0 {
            on_read(phy_addr)
        } else {
            Ok(self.chr_ram[phy_addr])
        }
    }
    fn ppu_write(&mut self, addr: u16, val: u8, on_write: &dyn Fn(usize, u8) -> WriteResult) -> WriteResult {
        if self.chr_rom_size > 0 {
            return Err("MMC1: Couldn't write to CHR ROM".into());
        }
        let phy_addr = self.map_chr_address(addr)?;
        self.chr_ram[phy_addr] = val;
        Ok(())
    }
}
