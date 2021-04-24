use crate::emulator::famicom::bus::{Bus, MAPPING_MODE_READ, MAPPING_MODE_WRITE, MAPPING_MODE_READ_WRITE, CPUBus, FakeMemory, OamDma};
use crate::emulator::famicom::cpu::CPU;
use crate::emulator::rom::ines::InesRom;
use crate::emulator::rom::cartridge::{CartridgeChrMemory, Cartridge, CartridgePrgMemory};
use std::rc::{Rc, Weak};
use std::cell::RefCell;
use std::borrow::BorrowMut;
use crate::emulator::famicom::ppu::{PPU, PPUMemory, CiRam, PaletteTable};
use crate::emulator::famicom::memory::RAM;
use std::ops::DerefMut;
use std::pin::Pin;
use std::marker::PhantomPinned;
use std::error::Error;
use crate::emulator::famicom::joypad::{ControllerMemory, Joypad};

#[derive(Debug)]
pub struct Famicom {
    pub cpu: CPU,
    pub ppu: PPU,
    pub cartridge: Weak<RefCell<Cartridge>>,
    pub joypads: [Joypad; 2],
    _pin: PhantomPinned,
}

impl Famicom {
    pub fn new() -> Self {
        let famicom = Self {
            cpu: CPU::new(),
            ppu: PPU::new(),
            cartridge: Weak::new(),
            joypads: [Joypad::new(), Joypad::new()],
            _pin: Default::default(),
        };
        famicom
    }

    pub fn new_pinned() -> Pin<Box<Self>> {
        let boxed = Box::pin(Self::new());
        boxed
    }

    pub fn reset(self: Pin<&mut Self>) {
        let this = unsafe { self.get_unchecked_mut() };
        this.cpu.reset();
    }

    pub fn step(self: Pin<&mut Self>) -> Result<(), Box<dyn Error>> {
        let this = unsafe { self.get_unchecked_mut() };
        let cycles = this.cpu.step()?;
        for i in 0..cycles * 3 {
            this.ppu.step()?;
        }
        Ok(())
    }


    pub fn unload(&mut self) {
        self.cpu.bus.clear_mappings();
        self.ppu.bus.clear_mappings();
    }

    pub fn loaded(&self) -> bool {
        self.cpu.bus.has_mappings() || self.ppu.bus.has_mappings()
    }

    pub fn load(self: Pin<&mut Self>, rom: &InesRom) {
        if self.loaded() {
            panic!("Already loaded")
        }
        let this = unsafe { self.get_unchecked_mut() };
        let ram = RAM::new();
        this.cpu.bus.add_mapping(0x0000, 0x2000, MAPPING_MODE_READ_WRITE, Box::new(ram));
        // self.cpu.bus.add_mapping(0x2000, 0x2000, MAPPING_MODE_READ_WRITE, Box::new(PPUMemory::new(self.ppu.deref_mut())));
        // self.cpu.bus.add_mapping(0x4014, 1, MAPPING_MODE_WRITE, Box::new(PPUMemory::new(self.ppu.deref_mut())));
        this.cpu.bus.attach_ppu(&mut this.ppu);
        this.cpu.bus.add_mapping(0x4000, 0x14, MAPPING_MODE_READ_WRITE, Box::new(FakeMemory));
        let oam = OamDma(&mut this.cpu.bus, &mut this.ppu);
        this.cpu.bus.add_mapping(0x4014, 1, MAPPING_MODE_WRITE, Box::new(oam));
        this.cpu.bus.add_mapping(0x4015, 1, MAPPING_MODE_READ_WRITE, Box::new(FakeMemory));
        this.cpu.bus.add_mapping(0x4016, 2, MAPPING_MODE_READ_WRITE, Box::new(ControllerMemory::new(&mut this.joypads)));
        let cartridge = Rc::new(RefCell::new(Cartridge::from_ines(rom)));
        this.cpu.bus.add_mapping(0x4020, 0xbfe0, MAPPING_MODE_READ_WRITE, Box::new(CartridgePrgMemory::new(cartridge.clone())));

        this.ppu.bus.add_mapping(0x0000, 0x2000, MAPPING_MODE_READ, Box::new(CartridgeChrMemory::new(cartridge.clone())));
        let nt_mirroring = cartridge.borrow().nt_mirroring;
        this.ppu.bus.add_mapping(0x2000, 0x1f00, MAPPING_MODE_READ_WRITE, Box::new(CiRam::new(nt_mirroring)));
        this.ppu.bus.add_mapping(0x3f00, 0x100, MAPPING_MODE_READ_WRITE, Box::new(PaletteTable::new()));

        *this.cartridge.borrow_mut() = Rc::downgrade(&cartridge);
    }

    pub fn cpu_mut(self: Pin<&mut Self>) -> &mut CPU {
        let this = unsafe { self.get_unchecked_mut() };
        &mut this.cpu
    }

    pub fn ppu_mut(self: Pin<&mut Self>) -> &mut PPU {
        let this = unsafe { self.get_unchecked_mut() };
        &mut this.ppu
    }

    pub fn joypads_mut(self: Pin<&mut Self>) -> &mut [Joypad; 2] {
        let this = unsafe { self.get_unchecked_mut() };
        &mut this.joypads
    }
}
