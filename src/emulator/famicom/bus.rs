use std::error::Error;
use std::rc::Rc;
use std::fmt::Debug;
use crate::emulator::famicom::memory::Memory;
use std::ops::{Deref, DerefMut};
use crate::emulator::famicom::ppu::{PPU, PPUMemory};
use std::ptr::null_mut;


pub const MAPPING_MODE_READ: usize = 1;
pub const MAPPING_MODE_WRITE: usize = 1 << 1;
pub const MAPPING_MODE_READ_WRITE: usize = MAPPING_MODE_READ | MAPPING_MODE_WRITE;

#[derive(Debug)]
pub struct Mapping {
    pub start: u16,
    pub length: u16,
    pub mode: usize,
    pub memory: Box<dyn Memory>,
}

#[derive(Debug)]
pub struct Bus {
    mappings: Vec<Mapping>,
}


impl Bus {
    pub fn new() -> Self {
        Bus {
            mappings: Vec::new()
        }
    }

    pub fn has_mappings(&self) -> bool {
        !self.mappings.is_empty()
    }

    pub fn clear_mappings(&mut self) {
        self.mappings.clear();
    }

    pub fn add_mapping(&mut self, start: u16, length: u16, mode: usize, memory: Box<dyn Memory>) {
        let mapping = Mapping {
            start,
            length,
            mode,
            memory,
        };
        self.mappings.push(mapping);
    }
    fn locate(&self, addr: u16) -> Option<(&Mapping, u16)> {
        let m = self.mappings.binary_search_by(|item| item.start.cmp(&addr));
        let index = match m {
            Ok(i) => i,
            Err(i) => i - 1,
        };
        if index < 0 {
            return None;
        }
        let mapping = &self.mappings[index];
        debug_assert!(mapping.start <= addr);
        if mapping.length < addr - mapping.start {
            return None;
        }
        Some((mapping, addr))
    }
    fn locate_mut(&mut self, addr: u16) -> Option<(&mut Mapping, u16)> {
        let m = self.mappings.binary_search_by(|item| item.start.cmp(&addr));
        let index = match m {
            Ok(i) => i,
            Err(i) => i - 1,
        };
        if index < 0 {
            return None;
        }
        let mapping = &mut self.mappings[index];
        debug_assert!(mapping.start <= addr);
        if mapping.length < addr - mapping.start {
            return None;
        }
        Some((mapping, addr))
    }
}

impl Memory for Bus {
    fn read(&self, addr: u16) -> Result<u8, Box<dyn Error>> {
        match self.locate(addr) {
            Some((mapping, phy_addr)) => {
                if mapping.mode & MAPPING_MODE_READ == 0 {
                    Err(format!("Permission denied when reading address ${:04x}", addr).into())
                } else {
                    mapping.memory.read(phy_addr)
                }
            }
            None => Err(format!("Attempted to read unmapped address ${:04x}", addr).into())
        }
    }

    fn read_u16(&self, addr: u16) -> Result<u16, Box<dyn Error>> {
        match self.locate(addr) {
            Some((mapping, phy_addr)) => {
                if mapping.mode & MAPPING_MODE_READ == 0 {
                    Err(format!("Permission denied when reading address ${:04x}", addr).into())
                } else {
                    mapping.memory.read_u16(phy_addr)
                }
            }
            None => Err(format!("Attempted to read unmapped address ${:04x}", addr).into())
        }
    }

    fn write(&mut self, addr: u16, val: u8) -> Result<(), Box<dyn Error>> {
        match self.locate_mut(addr) {
            Some((mapping, phy_addr)) => {
                if mapping.mode & MAPPING_MODE_WRITE == 0 {
                    Err(format!("Permission denied when writing address ${:04x}", addr).into())
                } else {
                    mapping.memory.write(phy_addr, val)
                }
            }
            None => Err(format!("Attempted to write unmapped address ${:04x}", addr).into())
        }
    }

    fn write_u16(&mut self, addr: u16, val: u16) -> Result<(), Box<dyn Error>> {
        match self.locate_mut(addr) {
            Some((mapping, phy_addr)) => {
                if mapping.mode & MAPPING_MODE_WRITE == 0 {
                    Err(format!("Permission denied when writing address ${:04x}", addr).into())
                } else {
                    mapping.memory.write_u16(phy_addr, val)
                }
            }
            None => Err(format!("Attempted to write unmapped address ${:04x}", addr).into())
        }
    }
}

#[derive(Debug)]
pub struct CPUBus(Bus, *mut PPU);

impl CPUBus {
    pub fn new() -> Self {
        Self(Bus::new(), null_mut())
    }

    pub fn attach_ppu(&mut self, ppu: *mut PPU) {
        self.1 = ppu;
        self.add_mapping(0x2000, 0x2000, MAPPING_MODE_READ_WRITE, Box::new(PPUMemory::new(ppu)));
        //self.add_mapping(0x4014, 1, MAPPING_MODE_WRITE, Box::new(PPUMemory::new(ppu)));
        // let oam = OamDma(self, ppu);
        // self.add_mapping(0x4014, 1, MAPPING_MODE_WRITE, Box::new(oam));
    }

    pub fn nmi_requested(&self) -> bool {
        unsafe { (*self.1).request_nmi }
    }

    pub fn clear_nmi(&mut self) {
        unsafe { (*self.1).request_nmi = false }
    }

    pub fn ppu_mut(&mut self) -> &mut PPU {
        unsafe { &mut *self.1 }
    }
    pub fn tick(&mut self, cycles: usize) {
        self.ppu_mut().run(cycles * 3);
    }
}

impl Deref for CPUBus {
    type Target = Bus;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for CPUBus {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Debug)]
pub struct OamDma(pub *mut CPUBus, pub *mut PPU);

impl Memory for OamDma {
    fn read(&self, addr: u16) -> Result<u8, Box<dyn Error>> {
        todo!()
    }

    fn write(&mut self, addr: u16, val: u8) -> Result<(), Box<dyn Error>> {
        let mut src_addr = (val as u16) << 8;
        unsafe {
            for i in 0..=0xff {
                let b = (*self.0).read(src_addr | i)?;
                (*self.1).spr_ram[i as usize] = b;
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct FakeMemory;

impl Memory for FakeMemory {
    fn read(&self, addr: u16) -> Result<u8, Box<dyn Error>> {
        Ok(0)
    }

    fn write(&mut self, addr: u16, val: u8) -> Result<(), Box<dyn Error>> {
        Ok(())
    }
}
