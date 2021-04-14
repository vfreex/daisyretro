use std::error::Error;
use std::fmt::Debug;

pub type Ptr = u16;

pub trait Memory: Debug {
    fn read(&self, addr: u16) -> Result<u8, Box<dyn Error>>;
    fn read_u16(&self, addr: u16) -> Result<u16, Box<dyn Error>> {
        let low = self.read(addr)?;
        let high = self.read(addr.wrapping_add(1))?;
        Ok((low as u16) | ((high as u16) << 8))
    }
    fn write(&mut self, addr: u16, val: u8) -> Result<(), Box<dyn Error>>;
    fn write_u16(&mut self, addr: u16, val: u16) -> Result<(), Box<dyn Error>> {
        let low = val as u8;
        let high = (val >> 8) as u8;
        self.write(addr, low)?;
        self.write(addr.wrapping_add(1), high)
    }
}

const RAM_SIZE: usize = 0x800;
#[derive(Debug)]
pub struct RAM([u8; RAM_SIZE]);

impl RAM {
    pub fn new() -> Self {
        RAM([0; RAM_SIZE])
    }
}

impl Memory for RAM {
    fn read(&self, addr: u16) -> Result<u8, Box<dyn Error>> {
        Ok(self.0[addr as usize & 0x7ff])
    }

    fn write(&mut self, addr: u16, val: u8) -> Result<(), Box<dyn Error>> {
        self.0[addr as usize & 0x7ff] = val;
        Ok(())
    }
}
