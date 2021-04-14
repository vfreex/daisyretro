use crate::emulator::famicom::bus::Bus;
use std::pin::Pin;
use std::ops::{DerefMut, Deref};
use std::rc::Rc;
use std::borrow::{BorrowMut, Borrow};
use std::cell::RefCell;
use crate::emulator::famicom::memory::Memory;
use std::error::Error;
use sdl2::video::FullscreenType::True;


#[derive(Debug)]
pub struct PPURegisters {
    pub ppu_ctrl: u8,
    pub ppu_mask: u8,
    pub ppu_status: u8,
    pub oam_addr: u8,
    pub oam_data: u8,
    pub ppu_scroll: u8,

    pub v: u16,
    pub t: u16,
    pub x: u8,
    pub w: bool,
    pub latch: u8,
}

impl PPURegisters {
    const PPU_STATUS_VBLANK: u8 = 1 << 7;
    pub fn new() -> Self {
        Self {
            ppu_ctrl: 0,
            ppu_mask: 0,
            ppu_status: 0,
            oam_addr: 0,
            oam_data: 0,
            ppu_scroll: 0,
            latch: 0,
            t: 0,
            v: 0,
            x: 0,
            w: false,
        }
    }
}

#[derive(Debug)]
pub struct PPU {
    pub bus: Bus,
    pub registers: PPURegisters,
    pub spr_ram: [u8; 0x100],
    cycle: usize,
    pub request_nmi: bool,
}

impl PPU {
    pub fn new() -> Self {
        Self {
            bus: Bus::new(),
            registers: PPURegisters::new(),
            spr_ram: [0; 0x100],
            cycle: 0,
            request_nmi: false,
        }
    }

    const SCANLINES_PER_FRAME: usize = 262;
    const DOTS_PER_SCANLINE: usize = 341;

    fn scanline(&self) -> usize {
        self.cycle / Self::DOTS_PER_SCANLINE % Self::SCANLINES_PER_FRAME
    }
    fn dot(&self) -> usize {
        self.cycle % Self::DOTS_PER_SCANLINE
    }

    // NES Classic palette colors from http://www.firebrandx.com/nespalette.html
    const PALETTE_COLORS: [(u8, u8, u8); 64] = [(0x61, 0x61, 0x61), (0x00, 0x00, 0x88), (0x1f, 0x0d, 0x99), (0x37, 0x13, 0x79), (0x56, 0x12, 0x60), (0x5d, 0x00, 0x10), (0x52, 0x0e, 0x00), (0x3a, 0x23, 0x08), (0x21, 0x35, 0x0c), (0x0d, 0x41, 0x0e), (0x17, 0x44, 0x17), (0x00, 0x3a, 0x1f), (0x00, 0x2f, 0x57), (0x00, 0x00, 0x00), (0x00, 0x00, 0x00), (0x00, 0x00, 0x00), (0xaa, 0xaa, 0xaa), (0x0d, 0x4d, 0xc4), (0x4b, 0x24, 0xde), (0x69, 0x12, 0xcf), (0x90, 0x14, 0xad), (0x9d, 0x1c, 0x48), (0x92, 0x34, 0x04), (0x73, 0x50, 0x05), (0x5d, 0x69, 0x13), (0x16, 0x7a, 0x11), (0x13, 0x80, 0x08), (0x12, 0x76, 0x49), (0x1c, 0x66, 0x91), (0x00, 0x00, 0x00), (0x00, 0x00, 0x00), (0x00, 0x00, 0x00), (0xfc, 0xfc, 0xfc), (0x63, 0x9a, 0xfc), (0x8a, 0x7e, 0xfc), (0xb0, 0x6a, 0xfc), (0xdd, 0x6d, 0xf2), (0xe7, 0x71, 0xab), (0xe3, 0x86, 0x58), (0xcc, 0x9e, 0x22), (0xa8, 0xb1, 0x00), (0x72, 0xc1, 0x00), (0x5a, 0xcd, 0x4e), (0x34, 0xc2, 0x8e), (0x4f, 0xbe, 0xce), (0x42, 0x42, 0x42), (0x00, 0x00, 0x00), (0x00, 0x00, 0x00), (0xfc, 0xfc, 0xfc), (0xbe, 0xd4, 0xfc), (0xca, 0xca, 0xfc), (0xd9, 0xc4, 0xfc), (0xec, 0xc1, 0xfc), (0xfa, 0xc3, 0xe7), (0xf7, 0xce, 0xc3), (0xe2, 0xcd, 0xa7), (0xda, 0xdb, 0x9c), (0xc8, 0xe3, 0x9e), (0xbf, 0xe5, 0xb8), (0xb2, 0xeb, 0xc8), (0xb7, 0xe5, 0xeb), (0xac, 0xac, 0xac), (0x00, 0x00, 0x00), (0x00, 0x00, 0x00)];

    pub fn render_screen(&mut self, screen: &mut [u8; 256 * 240 * 3]) -> Result<(), Box<dyn Error>> {
        let pt_bank = self.registers.ppu_ctrl >> 4 & 1;
        let pt_base = pt_bank as u16 * 0x1000;
        let nt_base_addr = 0x2000u16 + 0x400 * (self.registers.ppu_ctrl as u16 & 0x3);
        let attr_base_addr = nt_base_addr + 0x3c0;
        for scanline in 0..240usize {
            let tile_y = scanline / 8;
            for dot in 0..256usize {
                let tile_x = dot / 8;

                let tile_idx = tile_y * 32 + tile_x;
                let pt_ent_addr = nt_base_addr + tile_idx as u16;
                let tile_id = self.bus.read(pt_ent_addr)?;

                let col_in_tile = dot % 8;
                let row_in_tile = scanline % 8;
                let tile_addr = pt_base | (tile_id as u16 * 16 + row_in_tile as u16);
                let tile_low = self.bus.read(tile_addr)?;
                let tile_high = self.bus.read(tile_addr + 8)?;
                let color_low = (tile_low >> (7 - col_in_tile)) & 1;
                let color_high = (tile_high >> (7 - col_in_tile)) & 1;
                let color_index = (color_high << 1) | color_low;

                let palette = if color_index > 0 {
                    let tile_group_x = tile_x / 4;
                    let tile_group_y = tile_y / 4;
                    let tile_group = tile_group_y * 8 + tile_group_x;
                    let attr_addr = attr_base_addr + tile_group as u16;
                    let attr = self.bus.read(attr_addr)?;
                    match (tile_x % 4 / 2, tile_y % 4 / 2) {
                        (0, 0) /* top left */ => attr & 0x03,
                        (1, 0) /* top right */ => (attr >> 2) & 0x03,
                        (0, 1) /* bottom left */ => (attr >> 4) & 0x03,
                        (1, 1) /* bottom right */ => (attr >> 6) & 0x03,
                        _ => panic!()
                    }
                } else {
                    0
                };

                let color_addr = 0x3f00 | ((palette << 2) | color_index) as u16;
                let color = self.bus.read(color_addr)?;
                let rgb = Self::PALETTE_COLORS[color as usize];

                let index = ((scanline * 256 + dot) * 3) as usize;
                screen[index] = rgb.0;
                screen[index + 1] = rgb.1;
                screen[index + 2] = rgb.2;
            }
        }
        Ok(())
    }

    // pub fn render_tile(&self, pt_bank: usize, tile_id: u8, screen: &mut [u8; 256 * 240 * 3], x: usize, y: usize) -> Result<(), Box<dyn Error>> {
    //     debug_assert!(pt_bank < 2);
    //     let tile_addr = pt_bank as u16 * 0x1000 + tile_id as u16 * 16;
    //     for tile_row in 0..8usize {
    //         let tile_low = self.bus.read(tile_addr + tile_row as u16)?;
    //         let tile_high = self.bus.read(tile_addr + tile_row as u16 + 8)?;
    //         for tile_col in 0..8usize {
    //             let color_low = (tile_low >> (7 - tile_col)) & 1;
    //             let color_high = (tile_high >> (7 - tile_col)) & 1;
    //             let rgb = match (color_high, color_low) {
    //                 (0, 0) => Self::PALETTE_COLORS[0x01],
    //                 (0, 1) => Self::PALETTE_COLORS[0x23],
    //                 (1, 0) => Self::PALETTE_COLORS[0x27],
    //                 (1, 1) => Self::PALETTE_COLORS[0x30],
    //                 _ => panic!(),
    //             };
    //             let index = (((y + tile_row) * 256 + x + tile_col) * 3) as usize;
    //             screen[index] = rgb.0;
    //             screen[index + 1] = rgb.1;
    //             screen[index + 2] = rgb.2;
    //         }
    //     }
    //     Ok(())
    // }

    pub fn run(&mut self, steps: usize) -> Result<(), Box<dyn Error>> {
        for i in 0..steps {
            self.step()?;
        }
        Ok(())
    }

    pub fn step(&mut self) -> Result<(), Box<dyn Error>> {
        let scanline = self.scanline();
        let dot = self.dot();
        match scanline {
            261 => {
                if dot == 1 { // clear NMI
                    self.registers.ppu_status &= !PPURegisters::PPU_STATUS_VBLANK;
                    self.request_nmi = false;
                }
            }
            0..=239 => {}
            241 => {
                if dot == 1 { // set NMI
                    self.registers.ppu_status |= PPURegisters::PPU_STATUS_VBLANK;
                    if self.registers.ppu_ctrl & 0x80 != 0 {
                        self.request_nmi = true;
                    }
                }
            }
            _ => {}
        }
        self.cycle += 1; // FIXME: some kind of wrapping add needed
        Ok(())
    }
}

#[derive(Debug)]
pub struct PPUMemory(*mut PPU);

impl PPUMemory {
    pub fn new(ppu: *mut PPU) -> Self {
        Self(ppu)
    }
}

impl Memory for PPUMemory {
    fn read(&self, addr: u16) -> Result<u8, Box<dyn Error>> {
        let ppu = unsafe { &mut *self.0 };
        match addr & 0x2007 {
            0x2002 => { // PPU_STATUS
                let r = ppu.registers.ppu_status;
                if r & PPURegisters::PPU_STATUS_VBLANK != 0 {
                    println!("VBLANK notified.");
                }
                ppu.registers.ppu_status &= !PPURegisters::PPU_STATUS_VBLANK;
                ppu.registers.w = false;
                Ok(r)
            }
            0x2004 => { // OAM_DATA
                // The address is NOT auto-incremented after <reading> from 2004h.
                let r = ppu.spr_ram[ppu.registers.oam_addr as usize];
                Ok(r)
            }
            0x2007 => {  // PPU_DATA
                let r = if ppu.registers.v < 0x3f00 {
                    // Reading from VRAM 0000h-3EFFh loads the desired value into a latch,
                    // and returns the OLD content of the latch to the CPU
                    let r = ppu.registers.latch;
                    ppu.registers.latch = ppu.bus.read(ppu.registers.v as u16)?;
                    r
                } else {
                    // reading from Palette memory VRAM 3F00h-3FFFh does directly access the desired address.
                    let r = ppu.bus.read(ppu.registers.v as u16)?;
                    // reading the palettes still updates the internal buffer though,
                    // but the data placed in it is the mirrored nametable data that would appear "underneath" the palette
                    ppu.registers.latch = ppu.bus.read((ppu.registers.v & 0x2FFF) as u16)?;
                    r
                };
                // The PPU will auto-increment the VRAM address (selected via Port 2006h)
                // after each read/write from/to Port 2007h by 1 or 32 (depending on Bit2 of $2000).
                if ppu.registers.ppu_ctrl & 0b100 != 0 {
                    ppu.registers.v = ppu.registers.v.wrapping_add(32);
                } else {
                    ppu.registers.v = ppu.registers.v.wrapping_add(1);
                }
                Ok(r)
            }
            _ => Err(format!("PPU mapped address {:04x} is not readable", addr).into())
        }
    }

    fn write(&mut self, addr: u16, val: u8) -> Result<(), Box<dyn Error>> {
        let ppu = unsafe { &mut *self.0 };
        match addr & 0x2007 {
            0x2000 => { // PPU_CTRL
                ppu.registers.ppu_ctrl = val;
                ppu.registers.t = (ppu.registers.t & !0xc00) | (val as u16 & 0x03 << 10);
                Ok(())
            }
            0x2001 => { // PPU_MASK
                //ppu.registers.ppu_mask = val;
                Ok(())
            }
            0x2003 => { // OAM_ADDR
                ppu.registers.oam_addr = val;
                Ok(())
            }
            0x2004 => { // OAM_DATA
                ppu.spr_ram[ppu.registers.oam_addr as usize] = val;
                // // The Port 2003h address is auto-incremented by 1 after each <write> to 2004h.
                ppu.registers.oam_addr = ppu.registers.oam_addr.wrapping_add(1);
                Ok(())
            }
            0x2005 => { // PPU_SCROLL
                ppu.registers.ppu_scroll = val;
                if !ppu.registers.w {
                    ppu.registers.x = val & 0x07;
                    ppu.registers.t = (ppu.registers.t & !0x1f) | (val as u16 >> 3);
                } else {
                    let fine_y = val as u16 & 0x07;
                    ppu.registers.t = (ppu.registers.t & !0x73e0) | (val as u16 >> 3 << 5) | (fine_y << 12);
                }
                ppu.registers.w = !ppu.registers.w;
                Ok(())
            }
            0x2006 => { // PPUADDR
                if !ppu.registers.w {
                    let high = val & 0x3f;
                    ppu.registers.t = (ppu.registers.t & 0x00ff) | ((high as u16) << 8);
                } else {
                    let low = val;
                    ppu.registers.t = (ppu.registers.t & 0xff00) | (low as u16);
                    ppu.registers.v = ppu.registers.t;
                }
                ppu.registers.w = !ppu.registers.w;
                Ok(())
            }
            0x2007 => { // PPUDATA
                if ppu.registers.v >= 0x2000 && val != 0 {
                    println!("Writing PPUDATA at 0x{:04X}: {:02X}", ppu.registers.v, val);
                }
                ppu.bus.write(ppu.registers.v, val)?;
                // The PPU will auto-increment the VRAM address (selected via Port 2006h)
                // after each read/write from/to Port 2007h by 1 or 32 (depending on Bit2 of $2000).
                if ppu.registers.ppu_ctrl & 0b100 != 0 {
                    ppu.registers.v = ppu.registers.v.wrapping_add(32);
                } else {
                    ppu.registers.v = ppu.registers.v.wrapping_add(1);
                }
                Ok(())
            }
            _ => Err(format!("PPU mapped address {:04x} is not writable", addr).into())
        }
    }
}

#[derive(Debug)]
pub struct CiRam {
    data: [u8; 0x800]
}

impl CiRam {
    pub fn new() -> Self {
        Self {
            data: [0; 0x800]
        }
    }
}

impl Memory for CiRam {
    fn read(&self, addr: u16) -> Result<u8, Box<dyn Error>> {
        Ok(self.data[addr as usize & 0x07ff])
    }

    fn write(&mut self, addr: u16, val: u8) -> Result<(), Box<dyn Error>> {
        self.data[addr as usize & 0x07ff] = val;
        Ok(())
    }
}

#[derive(Debug)]
pub struct PaletteTable {
    data: [u8; 0x20]
}

impl PaletteTable {
    pub fn new() -> Self {
        Self {
            data: [0; 0x20]
        }
    }
    fn index(addr: u16) -> usize {
        let mut index = addr as usize & 0x1f;
        match addr & 0x1f {
            0x10 => 0x00,
            0x14 => 0x04,
            0x18 => 0x0c,
            0x1f => 0x0f,
            _ => index,
        }
    }
}

impl Memory for PaletteTable {
    fn read(&self, addr: u16) -> Result<u8, Box<dyn Error>> {
        let index = Self::index(addr);
        Ok(self.data[index])
    }

    fn write(&mut self, addr: u16, val: u8) -> Result<(), Box<dyn Error>> {
        let index = Self::index(addr);
        self.data[index] = val;
        Ok(())
    }
}
