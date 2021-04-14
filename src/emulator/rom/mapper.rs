use crate::emulator::rom::cartridge::Cartridge;
use std::error::Error;
use std::fmt::Debug;

type ReadResult = Result<u8, Box<dyn Error>>;
type WriteResult = Result<(), Box<dyn Error>>;
type R = Fn(u16) -> ReadResult;
type W = Fn(u16, u8) -> WriteResult;

pub trait Mapper: Debug {
    fn cpu_read(&self, addr: u16, on_read: &dyn Fn(u16) -> ReadResult) -> ReadResult;
    fn cpu_write(&mut self, addr: u16, val: u8, on_write: &dyn Fn(u16, u8) -> WriteResult) -> WriteResult;
    fn ppu_read(&self, addr: u16, on_read: &dyn Fn(u16) -> ReadResult) -> ReadResult;
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
    fn cpu_read(&self, addr: u16, on_read: &dyn Fn(u16) -> ReadResult) -> ReadResult {
        match addr {
            0x6000..=0x7fff => Ok(self.prg_ram[(addr - 0x6000) as usize]),
            0x8000..=0xbfff => {
                on_read(addr - 0x8000)
            }
            0xc000..=0xffff => {
                if addr as usize > self.prg_rom_size {
                    on_read(addr - 0xc000)
                } else {
                    on_read(addr - 0x8000)
                }
            }
            _ => Err(format!("Mapper #0 - Cannot read CPU memory address {:04x}: unmapped address", addr).into())
        }
    }

    fn cpu_write(&mut self, addr: u16, val: u8, on_write: &dyn Fn(u16, u8) -> WriteResult) -> WriteResult {
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

    fn ppu_read(&self, addr: u16, on_read: &dyn Fn(u16) -> ReadResult) -> ReadResult {
        match addr {
            0x0000..=0x2000 => {
                on_read(addr)
            }
            _ => Err(format!("Mapper #0 - Cannot read PPU memory address {:04x}: unmapped address", addr).into())
        }
    }
}
