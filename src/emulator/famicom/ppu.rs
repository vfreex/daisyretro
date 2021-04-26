use crate::emulator::famicom::bus::Bus;
use std::pin::Pin;
use std::ops::{DerefMut, Deref};
use std::rc::Rc;
use std::borrow::{BorrowMut, Borrow};
use std::cell::RefCell;
use crate::emulator::famicom::memory::Memory;
use std::error::Error;

bitflags! {
    pub struct PPUMaskFlags: u8 {
        const GREYSCALE = 1;
        const NO_BACKGROUND_CLIPPING = 1 << 1;
        const NO_SPIRIT_CLIPPING = 1 << 2;
        const BACKGROUND_VISIBILITY = 1 << 3;
        const SPIRIT_VISIBILITY = 1 << 4;
        const COLOR_EMPHASIZE_RED = 1 << 5;
	    const COLOR_EMPHASIZE_GREEN = 1 << 6;
	    const COLOR_EMPHASIZE_BLUE = 1 << 7;
    }
}

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

    pub bg_tile_latch: u8,
    pub bg_attr_low_latch: u8,
    pub bg_attr_high_latch: u8,
    pub bg_pattern_low_latch: u8,
    pub bg_pattern_high_latch: u8,

    pub bg_pattern_low_shift: u16,
    pub bg_pattern_high_shift: u16,
    pub bg_attr_low_shift: u16,
    pub bg_attr_high_shift: u16,

    pub latch: u8,
}

impl PPURegisters {
    const PPU_STATUS_VBLANK: u8 = 1 << 7;
    const PPU_STATUS_SPRITE_0_HIT: u8 = 1 << 6;
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
            bg_tile_latch: 0,
            bg_attr_low_latch: 0,
            bg_attr_high_latch: 0,
            bg_pattern_low_latch: 0,
            bg_pattern_high_latch: 0,
            bg_pattern_low_shift: 0,
            bg_pattern_high_shift: 0,
            bg_attr_low_shift: 0,
            bg_attr_high_shift: 0,
        }
    }
}

/// Sprite
/// https://wiki.nesdev.com/w/index.php/PPU_OAM
#[derive(Debug, Default)]
pub struct Sprite {
    pub x: u8,
    pub pattern_low: u8,
    pub pattern_high: u8,
    pub attr: u8,
}

bitflags! {
    pub struct SpriteAttrs: u8 {
        const PALATTE_ID = 0x03;
        const UNUSED = 0x1C;
        const PRIORITY = 1 << 5;
        const HORIZONTAL_FLIP = 1 << 6;
        const VERTICAL_FLIP = 1 << 7;
    }
}

#[derive(Debug)]
pub struct PPU {
    pub bus: Bus,
    pub registers: PPURegisters,
    pub spr_ram: [u8; 0x40 * 4],
    pub secondary_oam: [u8; 8 * 4],
    pub sprite_count: usize,
    pub active_sprites: [Sprite; 8],
    pub active_sprite_count: usize,
    cycle: u64,
    dot: usize,
    scanline: usize,
    frame: u64,
    pub request_nmi: bool,
    pub screen: [u8; 256 * 240 * 3],
}

impl PPU {
    pub fn new() -> Self {
        Self {
            bus: Bus::new(),
            registers: PPURegisters::new(),
            spr_ram: [0; 0x40 * 4],
            secondary_oam: [0; 8 * 4],
            sprite_count: 0,
            active_sprites: [Sprite::default(), Sprite::default(), Sprite::default(), Sprite::default(),
                Sprite::default(), Sprite::default(), Sprite::default(), Sprite::default()],
            active_sprite_count: 0,
            cycle: 0,
            dot: 0,
            scanline: 0,
            frame: 0,
            request_nmi: false,
            screen: [0; 256 * 240 * 3],
        }
    }

    const SCANLINES_PER_FRAME: usize = 262;
    const DOTS_PER_SCANLINE: usize = 341;

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
                let mut color_index = (color_high << 1) | color_low;

                let mut cur_palette = if color_index > 0 {
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
                let mut color_addr = 0x3f00 | ((cur_palette << 2) | color_index) as u16;

                let mut spr_px = 0;
                for spr in 0..64usize {
                    let spr_y = self.spr_ram[spr * 4] as usize + 1;
                    let spr_height;
                    if self.registers.ppu_ctrl >> 5 & 1 != 0 {
                        spr_height = 16;
                    } else {
                        spr_height = 8;
                    }
                    if scanline < spr_y || scanline >= spr_y + spr_height {
                        continue; // not in range
                    }
                    let mut dy = scanline - spr_y;
                    let spr_x = self.spr_ram[spr * 4 + 3] as usize;
                    if dot < spr_x as usize || dot >= spr_x as usize + 8 {
                        continue; // not in range
                    }
                    let mut dx = dot - spr_x;
                    let spr_tile = self.spr_ram[spr * 4 + 1];
                    let spr_attr = self.spr_ram[spr * 4 + 2];
                    let bank = self.registers.ppu_ctrl >> 3 & 1;
                    let addr = 0x1000 * bank as u16 | spr_tile as u16 * 16;
                    let palette = spr_attr & 3;
                    let bg_pri = spr_attr >> 5 & 1 != 0;
                    let hori_flip = spr_attr >> 6 & 1 != 0;
                    let vert_flip = spr_attr >> 7 & 1 != 0;
                    if bg_pri && color_index != 0 {
                        continue; // sprite px is behind bg
                    }
                    if hori_flip {
                        dx = 7 - dx;
                    }
                    if vert_flip {
                        dy = spr_height - 1 - dy;
                    }

                    let spr_low = self.bus.read(addr + dy as u16)?;
                    let spr_high = self.bus.read(addr + 8 + dy as u16)?;
                    spr_px = spr_low >> 7 - dx & 1 | spr_high >> 7 - dx & 1 << 1;
                    if spr_px != 0 {
                        color_index = spr_px;
                        cur_palette = palette;
                        color_addr = 0x3f10 | ((cur_palette << 2) | color_index) as u16;
                        break;
                    }
                }

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

    pub fn run(&mut self, steps: usize) -> Result<(usize), Box<dyn Error>> {
        for i in 0..steps {
            if self.request_nmi {
                return Ok(steps - i);
            }
            self.step()?;
        }
        Ok((0))
    }

    fn fetch_bg_tile(&mut self, dot: usize) -> Result<(), Box<dyn Error>> {
        match (dot - 1) % 8 + 1 {
            2 => { // fetch nametable byte
                let addr = 0x2000 | self.registers.v & 0x0fff;
                self.registers.bg_tile_latch = self.bus.read(addr)?;
            }
            4 => { // fetch attr byte
                let addr = 0x23c0 | (self.registers.v & 0x0c00) | ((self.registers.v >> 4) & 0x38) | ((self.registers.v >> 2) & 0x07);
                let attr = self.bus.read(addr)?;
                let coarse_x = self.registers.v & 0x1f;
                let coarse_y = (self.registers.v & 0x3e0) >> 5;
                let palette = match (coarse_y % 4 / 2, coarse_x % 4 / 2) {
                    (0, 0) /* top left */ => (attr >> 0) & 0x03,
                    (0, 1) /* top right */ => (attr >> 2) & 0x03,
                    (1, 0) /* bottom left */ => (attr >> 4) & 0x03,
                    (1, 1) /* bottom right */ => (attr >> 6) & 0x03,
                    _ => panic!("Impossible!"),
                };
                self.registers.bg_attr_low_latch = if palette & 1 != 0 { 0xff } else { 0 };
                self.registers.bg_attr_high_latch = if palette & 2 != 0 { 0xff } else { 0 };
            }
            6 => { // fetch tile bitmap low
                let fine_y = (self.registers.v >> 12) & 7;
                let mut addr = self.registers.bg_tile_latch as u16 * 16 + fine_y;
                if self.registers.ppu_ctrl & 0x10 != 0 {
                    addr |= 0x1000;
                }
                self.registers.bg_pattern_low_latch = self.bus.read(addr)?;
            }
            8 => {
                // fetch tile bitmap high
                let fine_y = self.registers.v >> 12 & 7;
                let mut addr = self.registers.bg_tile_latch as u16 * 16 + fine_y + 8;
                if self.registers.ppu_ctrl & 0x10 != 0 {
                    addr |= 0x1000;
                }
                self.registers.bg_pattern_high_latch = self.bus.read(addr)?;
                if dot == 256 {
                    // inc vert(v)
                    if self.registers.v & 0x7000 != 0x7000 { // fine_y < 7
                        self.registers.v += 0x1000; // fine_y++
                    } else { // inc coarse_y and set fine_y = 0
                        self.registers.v &= !0x7000;
                        let coarse_y = (self.registers.v & 0x3e0) >> 5;
                        if coarse_y == 29 { // set coarse_y = 0 and toggle nametable
                            self.registers.v &= !0x3e0;
                            self.registers.v ^= 0x800;
                        } else if coarse_y == 31 { // set coarse_y = 0 w/o toggling nametable
                            self.registers.v &= !0x3e0;
                        } else {
                            self.registers.v += 0x20; // coarse_y++
                        }
                    }
                } else {
                    // inc hori(v)
                    if self.registers.v & 0x1f < 31 {
                        self.registers.v += 1;
                    } else { // toggle nametable and reset coarse_x to 0
                        self.registers.v ^= 0x400;
                        self.registers.v &= !0x1f;
                    }
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn draw_pixel(&mut self, dot: usize, scanline: usize) -> Result<bool, Box<dyn Error>> {
        debug_assert!(dot >= 2 && dot < 258);
        debug_assert!(scanline < 240);
        let x = dot - 2;
        let y = scanline;

        let mut bg_pixel = 0;

        if self.registers.ppu_mask & 8 != 0 && (x >= 8 || self.registers.ppu_mask & 2 != 0) {
            let fine_x = self.registers.x;
            let bitmap_low = ((self.registers.bg_pattern_low_shift << fine_x >> 15) & 1) as u8;
            let bitmap_high = ((self.registers.bg_pattern_high_shift << fine_x >> 15) & 1) as u8;
            bg_pixel = bitmap_high << 1 | bitmap_low;

            if bg_pixel != 0 { // not universal bg color
                let attr_low = ((self.registers.bg_attr_low_shift << fine_x >> 15) & 1) as u8;
                let attr_high = ((self.registers.bg_attr_high_shift << fine_x >> 15) & 1) as u8;
                let palette = attr_high << 1 | attr_low;
                bg_pixel |= palette << 2;
            }
        }

        let mut bg_priority = false;
        let mut spr_pixel = 0;

        if self.registers.ppu_mask & 16 != 0 && (x >= 8 || self.registers.ppu_mask & 4 != 0) {
            for spr_idx in 0..self.active_sprite_count {
                let sprite = &mut self.active_sprites[spr_idx];
                if x < sprite.x as usize || x >= sprite.x as usize + 8 {
                    // sprite is not in range
                    continue;
                }
                let attr = sprite.attr;
                let mut dx = x as u8 - sprite.x;
                if attr & 64 != 0 { // horizontal flip
                    dx = 7 - dx;
                }
                let color_low = sprite.pattern_low >> (7 - dx) & 1;
                let color_high = sprite.pattern_high >> (7 - dx) & 1;
                spr_pixel = color_high << 1 | color_low;

                if spr_pixel != 0 { // sprite pixel is opaque
                    let palette = (attr as u8 & 3);
                    spr_pixel |= palette << 2 | 0x10;
                    bg_priority = attr & 32 != 0;
                    if spr_idx == 0 && bg_pixel != 0 && x < 255 {
                        self.registers.ppu_status |= PPURegisters::PPU_STATUS_SPRITE_0_HIT; // set Sprite0Hit
                    }
                    break; // the sprite data that occurs first will overlap any other sprites after it
                }
            }
        }

        let mut bitmap = if spr_pixel != 0 && (!bg_priority || bg_pixel == 0) {
            spr_pixel
        } else {
            bg_pixel
        };

        let color_addr = 0x3f00 | bitmap as u16;
        let color = self.bus.read(color_addr)?;
        let rgb = Self::PALETTE_COLORS[color as usize];
        let index = ((y * 256 + x) * 3) as usize;
        self.screen[index] = rgb.0;
        self.screen[index + 1] = rgb.1;
        self.screen[index + 2] = rgb.2;
        Ok(true)
    }

    // https://wiki.nesdev.com/w/index.php/PPU_sprite_evaluation
    fn evaluate_sprites(&mut self, dot: usize, scanline: usize) -> Result<(), Box<dyn Error>> {
        //let y = if scanline == Self::SCANLINES_PER_FRAME { 0 } else {scanline + 1};
        let y = scanline;
        match dot {
            1..=64 => {
                if dot % 2 == 0 { // clears the list of sprites to draw
                    self.secondary_oam[(dot - 1) / 2] = 0xff;
                }
            }
            //65..=256 => { // reads through OAM, checking which sprites will be on this scanline
            256 => {
                self.sprite_count = 0;
                self.registers.ppu_status &= !32;
                for spr_idx in 0..64usize {
                    if self.sprite_count < 8 {
                        let spr_y = self.spr_ram[spr_idx * 4];
                        // setting object scanline coordinates for ranges -1..-15 are actually interpreted as ranges 241..255
                        let delta_y = y as i16 - spr_y as i16;
                        if delta_y < 0 {
                            continue; // sprint is not in range
                        }
                        let spr_height = if self.registers.ppu_ctrl & 32 != 0 { 16 } else { 8 };
                        if delta_y >= spr_height {
                            continue;  // sprite is not in range
                        }
                        if self.sprite_count >= 8 {
                            // sprite overflow
                            // FIXME: hardware bug is not simulated for now
                            self.registers.ppu_status |= 32;
                            break;
                        }
                        // copy sprite from OAM to secondary_oam
                        self.secondary_oam[self.sprite_count * 4] = spr_y as u8;
                        self.secondary_oam[self.sprite_count * 4 + 1] = self.spr_ram[spr_idx * 4 + 1]; // tile index number
                        self.secondary_oam[self.sprite_count * 4 + 2] = self.spr_ram[spr_idx * 4 + 2]; // attributes
                        self.secondary_oam[self.sprite_count * 4 + 3] = self.spr_ram[spr_idx * 4 + 3]; // x position of left side of sprite.
                        self.sprite_count += 1;
                    }
                }
            }
            257..=320 => { // Sprite fetches (8 sprites total, 8 cycles per sprite
                let spr_id = (dot - 257) / 8;
                match dot % 8 {
                    0 => {
                        let spr_y = self.secondary_oam[spr_id * 4];
                        let byte1 = self.secondary_oam[spr_id * 4 + 1];
                        let attr = self.secondary_oam[spr_id * 4 + 2];
                        let spr_x = self.secondary_oam[spr_id * 4 + 3];
                        let tile_id;
                        let tile_bank;
                        let spr_height;
                        if self.registers.ppu_ctrl & 32 != 0 {  // 8 * 16 sprite tile
                            tile_id = byte1 & !1;
                            tile_bank = byte1 & 1;
                            spr_height = 16;
                        } else {
                            tile_id = byte1;
                            tile_bank = (self.registers.ppu_ctrl >> 3) & 1;
                            spr_height = 8;
                        }
                        let sprite = &mut self.active_sprites[spr_id];
                        let mut delta_y = y as i16 - spr_y as i16;
                        if attr & SpriteAttrs::VERTICAL_FLIP.bits != 0 { // vertical flip
                            delta_y = spr_height - 1 - delta_y;
                        }
                        let addr = tile_bank as u16 * 0x1000 | tile_id as u16 * 16 | delta_y as u16 | delta_y as u16 & 8;
                        sprite.x = spr_x;
                        sprite.pattern_low = self.bus.read(addr)?;
                        sprite.pattern_high = self.bus.read(addr + 8)?;
                        sprite.attr = attr;
                        self.active_sprite_count = self.sprite_count;
                    }
                    _ => {}
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn render(&mut self) -> Result<(), Box<dyn Error>> {
        let scanline = self.scanline;
        let dot = self.dot;
        match scanline { // https://wiki.nesdev.com/w/index.php/PPU_scrolling
            261 => { // pre-render line
                match dot {
                    257 => { // hori(v) = hori(t)
                        let mask = 0b0000010000011111;
                        self.registers.v &= !mask;
                        self.registers.v |= self.registers.t & mask;
                    }
                    280..=304 => { // vert(v) = vert(t)
                        let mask = 0b0111101111100000;
                        self.registers.v &= !mask;
                        self.registers.v |= self.registers.t & mask;
                    }
                    321..=336 => { // fetching the first two tiles for the next scanline
                        self.fetch_bg_tile(dot)?;
                    }
                    340 => {
                        if self.frame & 1 != 0 { // on every odd frame, scanline 0, dot 0 is skipped
                            self.scanline = 0;
                            self.dot = 0;
                        }
                    }
                    _ => {}
                }
            }
            0..=239 => { // visible lines
                // fetch tiles and attrs
                self.evaluate_sprites(dot, scanline)?;
                // fill shift registers
                match dot {
                    2..=257 | 322..=337 => {
                        if dot <= 257 { // draw pixel
                            self.draw_pixel(dot, scanline)?;
                        }
                        // The bg shift registers shift during dot 2..=257 and 322..=337.
                        self.registers.bg_pattern_low_shift <<= 1;
                        self.registers.bg_pattern_high_shift <<= 1;
                        self.registers.bg_attr_low_shift <<= 1;
                        self.registers.bg_attr_high_shift <<= 1;
                        if dot % 8 == 1 { // The bg shift registers are reloaded during ticks 9, 17, 25, ..., 257 and 329, 337.
                            self.registers.bg_pattern_low_shift |= self.registers.bg_pattern_low_latch as u16;
                            self.registers.bg_pattern_high_shift |= self.registers.bg_pattern_high_latch as u16;
                            self.registers.bg_attr_low_shift |= self.registers.bg_attr_low_latch as u16;
                            self.registers.bg_attr_high_shift |= self.registers.bg_attr_high_latch as u16;
                        }
                    }
                    _ => {}
                }
                match dot {
                    1..=256 => { // fetches 3rd..34th tile for the current scanline
                        self.fetch_bg_tile(dot)?;
                    }
                    257 => { // hori(v) = hori(t)
                        let mask = 0b0000010000011111;
                        self.registers.v &= !mask;
                        self.registers.v |= self.registers.t & mask;
                    }
                    321..=336 => { // fetching the first two tiles for the next scanline
                        self.fetch_bg_tile(dot)?;
                    }
                    _ => {}
                }
            }
            _ => {}
        }
        Ok(())
    }

    pub fn step(&mut self) -> Result<(), Box<dyn Error>> {
        let scanline = self.scanline;
        let dot = self.dot;
        match scanline {
            261 => { // pre-render line
                match dot {
                    1 => { // clear NMI
                        self.registers.ppu_status &= !(PPURegisters::PPU_STATUS_VBLANK | PPURegisters::PPU_STATUS_SPRITE_0_HIT);
                        self.request_nmi = false;
                        println!("VBLANK ended");
                    }
                    _ => {}
                }
            }
            241 => { // first v-blank line
                if dot == 1 && self.cycle > (Self::DOTS_PER_SCANLINE * Self::SCANLINES_PER_FRAME * 3) as u64 { // set NMI
                    self.registers.ppu_status |= PPURegisters::PPU_STATUS_VBLANK;
                    if self.registers.ppu_ctrl & 0x80 != 0 {
                        self.request_nmi = true;
                    }
                    println!("VBLANK started");
                }
            }
            _ => {}
        }
        if self.registers.ppu_mask & 0x18 != 0 {
            // render is enabled
            self.render()?;
        }
        self.cycle += 1;
        self.dot += 1;
        if self.dot >= Self::DOTS_PER_SCANLINE {
            self.dot = 0;
            self.scanline += 1;
            if self.scanline >= Self::SCANLINES_PER_FRAME {
                self.frame += 1;
                self.scanline = 0;
            }
        }
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
                let nt = val & 3;
                ppu.registers.t = (ppu.registers.t & !0xc00) | ((nt as u16) << 10);
                Ok(())
            }
            0x2001 => { // PPU_MASK
                ppu.registers.ppu_mask = val;
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
                    let coarse_y = val as u16 >> 3;
                    ppu.registers.t = (ppu.registers.t & !0x73e0) | (coarse_y << 5) | (fine_y << 12);
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
                // if ppu.registers.v >= 0x2000 && val != 0 {
                //     println!("Writing PPUDATA at 0x{:04X}: 0x{:02X}", ppu.registers.v, val);
                // }
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
    data: [u8; 0x1000],
    mirroring_map: *const [usize; 4],
}

impl CiRam {
    pub fn new(mirroring_map: *const [usize; 4]) -> Self {
        Self {
            data: [0; 0x1000],
            mirroring_map,
        }
    }

    fn map_addr(&self, addr: u16) -> usize {
        let logical = (addr & 0xfff) / 0x400;
        let physical = unsafe { self.mirroring_map.as_ref().unwrap()[logical as usize] };
        return (physical * 0x400) | (addr as usize & 0x3ff);
    }
}

impl Memory for CiRam {
    fn read(&self, addr: u16) -> Result<u8, Box<dyn Error>> {
        Ok(self.data[self.map_addr(addr)])
    }

    fn write(&mut self, addr: u16, val: u8) -> Result<(), Box<dyn Error>> {
        self.data[self.map_addr(addr)] = val;
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
        let index = addr as usize & 0x1f;
        match addr & 0x1f {
            0x10 | 0x14 | 0x18 | 0x1c => index & !0x10,
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
