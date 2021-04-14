use std::error::Error;
use crate::emulator::rom::ines::InesRom;
use crate::emulator::rom::mapper::{Mapper, NROMMapper};
use std::rc::Rc;
use std::cell::RefCell;
use crate::emulator::famicom::memory::Memory;

#[derive(Debug)]
pub struct Cartridge {
    pub prg_rom: Vec<u8>,
    pub chr_rom: Vec<u8>,
    pub mapper_id: u8,
    mapper: Box<dyn Mapper>,
}

impl Cartridge {
    pub fn from_ines(rom: &InesRom) -> Self {
        let mapper_id = rom.header.get_mapper_type();
        let mapper = match mapper_id {
            0 => Box::new(NROMMapper::new(rom.prg_rom.len(), rom.chr_rom.len(), 0)),
            _ => panic!(format!("Unsupported mapper type {}", mapper_id))
        };
        Cartridge {
            prg_rom: rom.prg_rom.to_vec(),
            chr_rom: rom.chr_rom.to_vec(),
            mapper_id,
            mapper,
        }
    }
}
#[derive(Debug)]
pub struct CartridgePrgMemory(Rc<RefCell<Cartridge>>);

impl CartridgePrgMemory {
    pub fn new(cartridge: Rc<RefCell<Cartridge>>) -> Self {
        Self(cartridge)
    }
}

impl Memory for CartridgePrgMemory {
    fn read(&self, addr: u16) -> Result<u8, Box<dyn Error>> {
        let cartridge = self.0.borrow();
        cartridge.mapper.cpu_read(addr, &|phy_addr| Ok(cartridge.prg_rom[phy_addr as usize]))
    }

    fn write(&mut self, addr: u16, val: u8) -> Result<(), Box<dyn Error>> {
        let mut cartridge = self.0.borrow_mut();
        cartridge.mapper.cpu_write(addr, val, &|a, v| Err("Write error".into()))
    }
}
#[derive(Debug)]
pub struct CartridgeChrMemory(Rc<RefCell<Cartridge>>);

impl CartridgeChrMemory {
    pub fn new(cartridge: Rc<RefCell<Cartridge>>) -> Self {
        Self(cartridge)
    }
}

impl Memory for CartridgeChrMemory {
    fn read(&self, addr: u16) -> Result<u8, Box<dyn Error>> {
        let cartridge = self.0.borrow();
        cartridge.mapper.ppu_read(addr, &|phy_addr| Ok(cartridge.chr_rom[phy_addr as usize]))
    }

    fn write(&mut self, addr: u16, val: u8) -> Result<(), Box<dyn Error>> {
        let cartridge = self.0.borrow();
        panic!("CHR ROM is read-only.");
    }
}
