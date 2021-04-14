use crate::emulator::famicom::memory::{Ptr, Memory};
use std::error::Error;
use std::iter::Map;
use std::borrow::BorrowMut;
pub use crate::emulator::famicom::cpu_instructions::*;
use std::sync::Arc;
use crate::emulator::famicom::bus::{Bus, CPUBus};

type Accumulator = u8;
type IndexRegister = u8;
type ProgramCounter = u16;
type StackPointer = u8;

#[derive(Debug, Default, Copy, Clone)]
pub struct ProcessorStatus(pub u8);

impl ProcessorStatus {
    pub const FLAG_CARRY: u8 = 1;
    pub const FLAG_ZERO: u8 = 1 << 1;
    pub const FLAG_INTERRUPT_DISABLE: u8 = 1 << 2;
    pub const FLAG_DECIMAL_MODE: u8 = 1 << 3;
    pub const FLAG_BREAK: u8 = 1 << 4;
    pub const FLAG_ONE: u8 = 1 << 5;
    pub const FLAG_OVERFLOW: u8 = 1 << 6;
    pub const FLAG_NEGATIVE: u8 = 1 << 7;

    #[inline(always)]
    pub fn get_flag(&self, flag: u8) -> bool {
        self.0 & flag != 0
    }
    #[inline(always)]
    pub fn set_flag_value(&mut self, flag: u8, val: bool) {
        if val {
            self.set_flag(flag);
        } else {
            self.clear_flag(flag);
        }
    }
    #[inline(always)]
    pub fn set_flag(&mut self, flag: u8) {
        self.0 |= flag
    }
    #[inline(always)]
    pub fn clear_flag(&mut self, flag: u8) {
        self.0 &= !flag
    }
}

#[derive(Debug, Default, Copy, Clone)]
pub struct Registers {
    pub A: Accumulator,
    pub X: IndexRegister,
    pub Y: IndexRegister,
    pub PC: ProgramCounter,
    pub S: StackPointer,
    pub P: ProcessorStatus,
}

#[derive(Debug, Copy, Clone)]
pub enum AddressingMode {
    Unsupported,
    Implicit,
    Accumulator,
    Immediate,
    ZeroPage,
    ZeroPage_X,
    ZeroPage_Y,
    Relative,
    Absolute,
    Absolute_X,
    Absolute_Y,
    Indirect,
    Indirect_X,
    Indirect_Y,
}

#[derive(Debug)]
pub struct CPU {
    pub registers: Registers,
    pub bus: CPUBus,
}

impl CPU {
    pub fn new() -> Self {
        // https://wiki.nesdev.com/w/index.php/CPU_ALL#At_power-up
        let registers = Registers {
            A: 0,
            X: 0,
            Y: 0,
            PC: 0,
            S: 0,
            P: ProcessorStatus(0),
        };
        let cpu = Self {
            registers,
            bus: CPUBus::new(),
        };
        cpu
    }

    pub const IV_NMI: u16 = 0xFFFA;
    pub const IV_RESET: u16 = 0xFFFC;
    pub const IV_IRQ_BRK: u16 = 0xFFFE;

    pub fn reset(&mut self) {
        // https://wiki.nesdev.com/w/index.php/CPU_ALL#After_reset
        self.registers.S = self.registers.S.wrapping_sub(3);
        self.registers.P.set_flag(ProcessorStatus::FLAG_INTERRUPT_DISABLE | ProcessorStatus::FLAG_ONE);
        self.registers.PC = self.bus.read_u16(Self::IV_RESET).unwrap();
    }

    fn interrupt(&mut self, int_vec: u16) {
        self.push_stack_u16(self.registers.PC).unwrap();
        let p = (self.registers.P.0 | ProcessorStatus::FLAG_ONE) & !ProcessorStatus::FLAG_BREAK;
        self.push_stack(p).unwrap();
        self.registers.P.set_flag_value(ProcessorStatus::FLAG_INTERRUPT_DISABLE, true);
        self.registers.PC = self.bus.read_u16(int_vec).unwrap();
    }

    fn address_operand(&mut self, addressing_mode: AddressingMode) -> Result<Ptr, Box<dyn Error>> {
        let reg = &mut self.registers;
        let pc = reg.PC;
        match addressing_mode {
            AddressingMode::Implicit | AddressingMode::Accumulator => Ok(0),
            AddressingMode::Immediate => {
                reg.PC = reg.PC.wrapping_add(1);
                Ok(pc)
            }
            AddressingMode::ZeroPage => {
                reg.PC = reg.PC.wrapping_add(1);
                Ok(self.bus.read(pc)? as Ptr)
            }
            AddressingMode::ZeroPage_X => {
                reg.PC = reg.PC.wrapping_add(1);
                Ok((self.bus.read(pc)?.wrapping_add(self.registers.X)) as Ptr)
            }
            AddressingMode::ZeroPage_Y => {
                reg.PC = reg.PC.wrapping_add(1);
                Ok((self.bus.read(pc)?.wrapping_add(self.registers.Y)) as Ptr)
            }
            AddressingMode::Relative => {
                let offset = self.bus.read(pc)? as i8 as Ptr;
                reg.PC = reg.PC.wrapping_add(1);
                Ok(reg.PC.wrapping_add(offset))
            }
            AddressingMode::Absolute => {
                reg.PC = reg.PC.wrapping_add(2);
                Ok(self.bus.read_u16(pc)?)
            }
            AddressingMode::Absolute_X => {
                reg.PC = reg.PC.wrapping_add(2);
                Ok(self.bus.read_u16(pc)?.wrapping_add(self.registers.X as Ptr))
            }
            AddressingMode::Absolute_Y => {
                reg.PC = reg.PC.wrapping_add(2);
                Ok(self.bus.read_u16(pc)?.wrapping_add(self.registers.Y as Ptr))
            }
            AddressingMode::Indirect => {
                reg.PC = reg.PC.wrapping_add(2);
                let addr_low = self.bus.read_u16(pc)?;
                let addr_high = (addr_low & 0xff00) | ((addr_low + 1) & 0xff); // 6502 CPU bug
                let low = self.bus.read(addr_low)?;
                let high = self.bus.read(addr_high)?;
                Ok((high as Ptr) << 8 | (low as Ptr))
            }
            AddressingMode::Indirect_X => {
                reg.PC = reg.PC.wrapping_add(1);
                let addr = self.bus.read(pc)?.wrapping_add(self.registers.X);
                let low = self.bus.read(addr as Ptr)?;
                let high = self.bus.read(addr.wrapping_add(1) as Ptr)?;
                Ok((high as Ptr) << 8 | (low as Ptr))
            }
            AddressingMode::Indirect_Y => {
                reg.PC = reg.PC.wrapping_add(1);
                let addr = self.bus.read(pc)?;
                let low = self.bus.read(addr as Ptr)?;
                let high = self.bus.read(addr.wrapping_add(1) as Ptr)?;
                Ok(((high as Ptr) << 8 | (low as Ptr)).wrapping_add(self.registers.Y as Ptr))
            }
            _ => return Err(format!("Unsupported opcode").into())
        }
    }

    #[inline(always)]
    fn decode_opcode(&self) -> Result<&Instruction, Box<dyn Error>> {
        let pc = self.registers.PC;
        let opcode = self.bus.read(pc)?;
        if let Some(ref op) = SUPPORTED_INSTRUCTIONS[opcode as usize] {
            debug_assert_eq!(op.opcode, opcode);
            let op1 = self.bus.read(pc.wrapping_add(1))?;
            let op2 = self.bus.read(pc.wrapping_add(2))?;
            let dump_regs = format!("A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X}",
                                    self.registers.A, self.registers.X, self.registers.Y, self.registers.P.0, self.registers.S);
            //println!("{:04X}  {}  {}", pc, op.to_string(op1, op2), dump_regs);
            Ok(op)
        } else {
            Err(format!("Unsupported opcode {} at ${:04x}", opcode, pc).into())
        }
    }

    pub fn run(&mut self, cycle_budget: usize) -> Result<usize, Box<dyn Error>> {
        let mut remain = cycle_budget;
        loop {
            if self.bus.nmi_requested() {
                self.bus.clear_nmi();
                print!("about to handle NMI");
                self.interrupt(Self::IV_NMI);
                //self.bus.tick(2);
                return Ok(remain); // FIXME: dummy cycle value
            }
            let &Instruction { addressing_mode, cycles, handler, .. } = self.decode_opcode()?;
            if remain < cycles {
                break;
            }
            self.registers.PC = self.registers.PC.wrapping_add(1);
            let operand = self.address_operand(addressing_mode)?;
            handler(self, operand)?;
            self.bus.tick(cycles);
            remain -= cycles;
        }
        Ok(remain)
    }
    pub fn step(&mut self) -> Result<usize, Box<dyn Error>> {
        if self.bus.nmi_requested() {
            self.bus.clear_nmi();
            self.interrupt(Self::IV_NMI);
            //self.bus.tick(2);
            return Ok(1); // FIXME: dummy cycle value
        }
        let &Instruction { addressing_mode, cycles, handler, .. } = self.decode_opcode()?;
        self.registers.PC = self.registers.PC.wrapping_add(1);
        let operand = self.address_operand(addressing_mode)?;
        handler(self, operand)?;
        self.bus.tick(cycles);
        Ok(cycles)
    }
}
