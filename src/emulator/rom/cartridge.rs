use std::error::Error;
use crate::emulator::rom::ines::{InesRom, InesHeader};
use crate::emulator::rom::mapper::{Mapper, NROMMapper, Mmc1Mapper};
use std::rc::Rc;
use std::cell::RefCell;
use crate::emulator::famicom::memory::Memory;

#[derive(Debug)]
pub struct Cartridge {
    pub prg_rom: Vec<u8>,
    pub chr_rom: Vec<u8>,
    pub mapper_id: u8,
    mapper: Box<dyn Mapper>,
    pub nt_mirroring: [usize; 4],
}

impl Cartridge {
    pub fn from_ines(rom: &InesRom) -> Self {
        let mapper_id = rom.header.get_mapper_type();
        let mapper: Box<dyn Mapper> = match mapper_id {
            0 => Box::new(NROMMapper::new(rom.prg_rom.len(), rom.chr_rom.len(), 0)),
            1 => Box::new(Mmc1Mapper::new(rom.prg_rom.len(), rom.chr_rom.len(), 0)),
            _ => panic!(format!("Unsupported mapper type {}", mapper_id))
        };
        let nt_mirroring = if rom.header.four_screen_vram_present() {
            [0, 1, 2, 3]
        } else if rom.header.vertical_mirroring_set() {
            [0, 1, 0, 1]
        } else {
            [0, 0, 1, 1]
        };
        Cartridge {
            prg_rom: rom.prg_rom.to_vec(),
            chr_rom: rom.chr_rom.to_vec(),
            mapper_id,
            mapper,
            nt_mirroring,
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
        let mut mirroring = &mut cartridge.nt_mirroring as *mut [usize; 4];

        cartridge.mapper.cpu_write(addr, val,
                                   &|a, v| Err("Write error".into()),
                                   &mut |v| Ok(unsafe { mirroring.as_mut().unwrap().copy_from_slice(&v) }))
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
        let mut cartridge = self.0.borrow_mut();
        cartridge.mapper.ppu_write(addr, val,
                                   &|a, v| Err("Write error".into()))
    }
}
