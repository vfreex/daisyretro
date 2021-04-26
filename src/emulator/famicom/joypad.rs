use crate::emulator::famicom::memory::Memory;
use std::error::Error;

#[derive(Debug)]
pub struct Joypad {
    pressed_keys: u8,
    shifts: u8,
}

impl Joypad {
    pub const BUTTON_A: u8 = 1;
    pub const BUTTON_B: u8 = 1 << 1;
    pub const BUTTON_SELECT: u8 = 1 << 2;
    pub const BUTTON_START: u8 = 1 << 3;
    pub const BUTTON_UP: u8 = 1 << 4;
    pub const BUTTON_DOWN: u8 = 1 << 5;
    pub const BUTTON_LEFT: u8 = 1 << 6;
    pub const BUTTON_RIGHT: u8 = 1 << 7;

    pub fn new() -> Self {
        Self {
            pressed_keys: 0,
            shifts: 0,
        }
    }

    pub fn set(&mut self, keys: u8) {
        self.pressed_keys = keys;
    }

    pub fn get(&self) -> u8 {
        self.pressed_keys
    }

    pub fn press(&mut self, keys: u8) {
        self.pressed_keys |= keys;
    }
    pub fn release(&mut self, keys: u8) {
        self.pressed_keys &= !keys;
    }

}


#[derive(Debug)]
pub struct ControllerMemory {
    pads: *mut [Joypad; 2],
    strobe: bool,
}

impl ControllerMemory {
    pub fn new(pads: *mut [Joypad; 2]) -> Self {
        Self {
            pads,
            strobe: false,
        }
    }

    pub fn joypads(&self) -> &[Joypad; 2] {
        unsafe { &mut *self.pads }
    }

    pub fn joypads_mut(&mut self) -> &mut [Joypad; 2] {
        unsafe { &mut *self.pads }
    }

    pub fn joypad(&self, index: usize) -> &Joypad {
        let p = self.joypads();
        &p[index]
    }
    pub fn joypad_mut(&mut self, index: usize) -> &mut Joypad {
        let p = self.joypads_mut();
        &mut p[index]
    }
}

impl Memory for ControllerMemory {
    fn read(&self, addr: u16) -> Result<u8, Box<dyn Error>> {
        let this = unsafe { &mut *(self as *const Self as *mut Self) };
        match addr {
            0x4016..=0x4017 => {
                let pad = this.joypad_mut(addr as usize & 1);
                let mut v = 1;
                if pad.shifts < 8 {
                    v = (pad.pressed_keys >> pad.shifts) & 1;
                    if !self.strobe {
                        pad.shifts += 1;
                    }
                }
                Ok(v)
            }
            _ => Err(format!("Reading controller mapped memory {:04x} is not permitted.", addr).into())
        }
    }

    fn write(&mut self, addr: u16, val: u8) -> Result<(), Box<dyn Error>> {
        match addr {
            0x4016 => {
                self.strobe = val & 1 != 0;
                if self.strobe {
                    self.joypads_mut().iter_mut().for_each(|pad| pad.shifts = 0);
                }
                Ok(())
            }
            0x4017 => {
                println!("Ignore writing to ${:04x}", addr);
                Ok(())
            }
            _ => Err(format!("Writing controller mapped memory {:04x} is not permitted.", addr).into())
        }
    }
}
