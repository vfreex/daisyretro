use crate::emulator::famicom::cpu::*;
use std::error::Error;
use crate::emulator::famicom::memory::Ptr;
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::fmt;
use std::sync::Arc;
use crate::emulator::famicom::memory::Memory;

type InstructionHandler = fn(&mut CPU, Ptr) -> Result<(), Box<dyn Error>>;

pub struct Instruction {
    pub opcode: u8,
    pub mnemonic: &'static str,
    pub addressing_mode: AddressingMode,
    pub length: usize,
    pub cycles: usize,
    pub optional_cycles: usize,
    pub handler: InstructionHandler,
}

impl Instruction {
    pub fn to_string(&self, operand1: u8, operand2: u8) -> String {
        let operand_str: String = match self.addressing_mode {
            AddressingMode::Unsupported => " <unknown>".into(),
            AddressingMode::Implicit => "".into(),
            AddressingMode::Accumulator => " A".into(),
            AddressingMode::Immediate => format!(" #${:02X}", operand1),
            AddressingMode::ZeroPage => format!(" ${:02X}", operand1),
            AddressingMode::ZeroPage_X => format!(" ${:02X},X", operand1),
            AddressingMode::ZeroPage_Y => format!(" ${:02X},Y", operand1),
            AddressingMode::Relative => format!(" *{}", operand1 as i8),
            AddressingMode::Absolute => format!(" ${:02X}{:02X}", operand2, operand1),
            AddressingMode::Absolute_X => format!(" ${:02X}{:02X},X", operand2, operand1),
            AddressingMode::Absolute_Y => format!(" ${:02X}{:02X},Y", operand2, operand1),
            AddressingMode::Indirect => format!(" (${:02X}{:02X})", operand2, operand1),
            AddressingMode::Indirect_X => format!(" (${:02X},X)", operand1),
            AddressingMode::Indirect_Y => format!(" (${:02X}),Y", operand1),
        };
        let hex = match self.length {
            2 => format!("{:02x} {:02x}   ", self.opcode, operand1),
            3 => format!("{:02x} {:02x} {:02x}", self.opcode, operand1, operand2),
            _ => format!("{:02x}      ", self.opcode),
        };
        let s = format!("{}  {}{}", hex, self.mnemonic, operand_str);
        //let s = self.mnemonic.to_owned() + operand_str.as_str() + raw.as_str();
        s
    }

    pub fn new(opcode: u8, mnemonic: &'static str, addressing_mode: AddressingMode, length: usize, cycles: usize, optional_cycles: usize, handler: InstructionHandler) -> Self {
        Instruction {
            opcode,
            mnemonic,
            addressing_mode,
            length,
            cycles,
            optional_cycles,
            handler,
        }
    }
}

lazy_static! {
    pub static ref SUPPORTED_INSTRUCTIONS: [Option<Instruction>; 256] = {
        const Unsupported: Option<Instruction> = None;
        let mut instructions = [Unsupported; 256];
        let mut register = |opcode: u8, mnemonic: &'static str, addressing_mode: AddressingMode, length: usize, cycles: usize, optional_cycles: usize, handler: InstructionHandler | {
            let instruction = Instruction::new(opcode, mnemonic, addressing_mode, length, cycles, optional_cycles, handler);
            instructions[opcode as usize] = Some(instruction);
        };
        // LDA
        register(0xa9, "LDA", AddressingMode::Immediate, 2, 2, 0, CPU::lda);
        register(0xa5, "LDA", AddressingMode::ZeroPage, 2, 3, 0, CPU::lda);
        register(0xb5, "LDA", AddressingMode::ZeroPage_X, 2, 4, 0, CPU::lda);
        register(0xad, "LDA", AddressingMode::Absolute, 3, 4, 0, CPU::lda);
        register(0xbd, "LDA", AddressingMode::Absolute_X, 3, 4, 1, CPU::lda);
        register(0xb9, "LDA", AddressingMode::Absolute_Y, 3, 4, 1, CPU::lda);
        register(0xa1, "LDA", AddressingMode::Indirect_X, 2, 6, 0, CPU::lda);
        register(0xb1, "LDA", AddressingMode::Indirect_Y, 2, 5, 1, CPU::lda);

        // LDX
        register(0xa2, "LDX", AddressingMode::Immediate, 2, 2, 0, CPU::ldx);
        register(0xa6, "LDX", AddressingMode::ZeroPage, 2, 3, 0, CPU::ldx);
        register(0xb6, "LDX", AddressingMode::ZeroPage_Y, 2, 4, 0, CPU::ldx);
        register(0xae, "LDX", AddressingMode::Absolute, 3, 4, 0, CPU::ldx);
        register(0xbe, "LDX", AddressingMode::Absolute_Y, 3, 4, 1, CPU::ldx);

        // LDY
        register(0xa0, "LDY", AddressingMode::Immediate, 2, 2, 0, CPU::ldy);
        register(0xa4, "LDY", AddressingMode::ZeroPage, 2, 3, 0, CPU::ldy);
        register(0xb4, "LDY", AddressingMode::ZeroPage_X, 2, 4, 0, CPU::ldy);
        register(0xac, "LDY", AddressingMode::Absolute, 3, 4, 0, CPU::ldy);
        register(0xbc, "LDY", AddressingMode::Absolute_X, 3, 4, 1, CPU::ldy);

        // STA
        register(0x85, "STA", AddressingMode::ZeroPage, 2, 3, 0, CPU::sta);
        register(0x95, "STA", AddressingMode::ZeroPage_X, 2, 4, 0, CPU::sta);
        register(0x8d, "STA", AddressingMode::Absolute, 3, 4, 0, CPU::sta);
        register(0x9d, "STA", AddressingMode::Absolute_X, 3, 5, 0, CPU::sta);
        register(0x99, "STA", AddressingMode::Absolute_Y, 3, 5, 0, CPU::sta);
        register(0x81, "STA", AddressingMode::Indirect_X, 2, 6, 0, CPU::sta);
        register(0x91, "STA", AddressingMode::Indirect_Y, 2, 6, 0, CPU::sta);

        // STX
        register(0x86, "STX", AddressingMode::ZeroPage, 2, 3, 0, CPU::stx);
        register(0x96, "STX", AddressingMode::ZeroPage_Y, 2, 4, 0, CPU::stx);
        register(0x8e, "STX", AddressingMode::Absolute, 3, 4, 0, CPU::stx);

        // STY
        register(0x84, "STY", AddressingMode::ZeroPage, 2, 3, 0, CPU::sty);
        register(0x94, "STY", AddressingMode::ZeroPage_X, 2, 4, 0, CPU::sty);
        register(0x8c, "STY", AddressingMode::Absolute, 3, 4, 0, CPU::sty);

        // TAX
        register(0xaa, "TAX", AddressingMode::Implicit, 1, 2, 0, CPU::tax);
        // TAY
        register(0xa8, "TAY", AddressingMode::Implicit, 1, 2, 0, CPU::tay);
        // TXA
        register(0x8a, "TXA", AddressingMode::Implicit, 1, 2, 0, CPU::txa);
        // TYA
        register(0x98, "TYA", AddressingMode::Implicit, 1, 2, 0, CPU::tya);

        // TSX
        register(0xba, "TSX", AddressingMode::Implicit, 1, 2, 0, CPU::tsx);
        // TXS
        register(0x9a, "TXS", AddressingMode::Implicit, 1, 2, 0, CPU::txs);
        // PHA
        register(0x48, "PHA", AddressingMode::Implicit, 1, 3, 0, CPU::pha);
        // PHP
        register(0x08, "PHP", AddressingMode::Implicit, 1, 3, 0, CPU::php);
        // PLA
        register(0x68, "PLA", AddressingMode::Implicit, 1, 4, 0, CPU::pla);
        // PLP
        register(0x28, "PLP", AddressingMode::Implicit, 1, 4, 0, CPU::plp);

        // AND
        register(0x29, "AND", AddressingMode::Immediate, 2, 2, 0, CPU::and);
        register(0x25, "AND", AddressingMode::ZeroPage, 2, 3, 0, CPU::and);
        register(0x35, "AND", AddressingMode::ZeroPage_X, 2, 4, 0, CPU::and);
        register(0x2d, "AND", AddressingMode::Absolute, 3, 4, 0, CPU::and);
        register(0x3d, "AND", AddressingMode::Absolute_X, 3, 4, 1, CPU::and);
        register(0x39, "AND", AddressingMode::Absolute_Y, 3, 4, 1, CPU::and);
        register(0x21, "AND", AddressingMode::Indirect_X, 2, 6, 0, CPU::and);
        register(0x31, "AND", AddressingMode::Indirect_Y, 2, 5, 1, CPU::and);

        // EOR
        register(0x49, "EOR", AddressingMode::Immediate, 2, 2, 0, CPU::eor);
        register(0x45, "EOR", AddressingMode::ZeroPage, 2, 3, 0, CPU::eor);
        register(0x55, "EOR", AddressingMode::ZeroPage_X, 2, 4, 0, CPU::eor);
        register(0x4d, "EOR", AddressingMode::Absolute, 3, 4, 0, CPU::eor);
        register(0x5d, "EOR", AddressingMode::Absolute_X, 3, 4, 1, CPU::eor);
        register(0x59, "EOR", AddressingMode::Absolute_Y, 3, 4, 1, CPU::eor);
        register(0x41, "EOR", AddressingMode::Indirect_X, 2, 6, 0, CPU::eor);
        register(0x51, "EOR", AddressingMode::Indirect_Y, 2, 5, 1, CPU::eor);

        // ORA
        register(0x09, "ORA", AddressingMode::Immediate, 2, 2, 0, CPU::ora);
        register(0x05, "ORA", AddressingMode::ZeroPage, 2, 3, 0, CPU::ora);
        register(0x15, "ORA", AddressingMode::ZeroPage_X, 2, 4, 0, CPU::ora);
        register(0x0d, "ORA", AddressingMode::Absolute, 3, 4, 0, CPU::ora);
        register(0x1d, "ORA", AddressingMode::Absolute_X, 3, 4, 1, CPU::ora);
        register(0x19, "ORA", AddressingMode::Absolute_Y, 3, 4, 1, CPU::ora);
        register(0x01, "ORA", AddressingMode::Indirect_X, 2, 6, 0, CPU::ora);
        register(0x11, "ORA", AddressingMode::Indirect_Y, 2, 5, 1, CPU::ora);

        // BIT
        register(0x24, "BIT", AddressingMode::ZeroPage, 2, 3, 0, CPU::bit);
        register(0x2c, "BIT", AddressingMode::Absolute, 3, 4, 0, CPU::bit);

        // ADC
        register(0x69, "ADC", AddressingMode::Immediate, 2, 2, 0, CPU::adc);
        register(0x65, "ADC", AddressingMode::ZeroPage, 2, 3, 0, CPU::adc);
        register(0x75, "ADC", AddressingMode::ZeroPage_X, 2, 4, 0, CPU::adc);
        register(0x6d, "ADC", AddressingMode::Absolute, 3, 4, 0, CPU::adc);
        register(0x7d, "ADC", AddressingMode::Absolute_X, 3, 4, 1, CPU::adc);
        register(0x79, "ADC", AddressingMode::Absolute_Y, 3, 4, 1, CPU::adc);
        register(0x61, "ADC", AddressingMode::Indirect_X, 2, 6, 0, CPU::adc);
        register(0x71, "ADC", AddressingMode::Indirect_Y, 2, 5, 1, CPU::adc);

        // SBC
        register(0xe9, "SBC", AddressingMode::Immediate, 2, 2, 0, CPU::sbc);
        register(0xe5, "SBC", AddressingMode::ZeroPage, 2, 3, 0, CPU::sbc);
        register(0xf5, "SBC", AddressingMode::ZeroPage_X, 2, 4, 0, CPU::sbc);
        register(0xed, "SBC", AddressingMode::Absolute, 3, 4, 0, CPU::sbc);
        register(0xfd, "SBC", AddressingMode::Absolute_X, 3, 4, 1, CPU::sbc);
        register(0xf9, "SBC", AddressingMode::Absolute_Y, 3, 4, 1, CPU::sbc);
        register(0xe1, "SBC", AddressingMode::Indirect_X, 2, 6, 0, CPU::sbc);
        register(0xf1, "SBC", AddressingMode::Indirect_Y, 2, 5, 1, CPU::sbc);

        // CMP
        register(0xc9, "CMP", AddressingMode::Immediate, 2, 2, 0, CPU::cmp);
        register(0xc5, "CMP", AddressingMode::ZeroPage, 2, 3, 0, CPU::cmp);
        register(0xd5, "CMP", AddressingMode::ZeroPage_X, 2, 4, 0, CPU::cmp);
        register(0xcd, "CMP", AddressingMode::Absolute, 3, 4, 0, CPU::cmp);
        register(0xdd, "CMP", AddressingMode::Absolute_X, 3, 4, 1, CPU::cmp);
        register(0xd9, "CMP", AddressingMode::Absolute_Y, 3, 4, 1, CPU::cmp);
        register(0xc1, "CMP", AddressingMode::Indirect_X, 2, 6, 0, CPU::cmp);
        register(0xd1, "CMP", AddressingMode::Indirect_Y, 2, 5, 1, CPU::cmp);

        // CPX
        register(0xe0, "CPX", AddressingMode::Immediate, 2, 2, 0, CPU::cpx);
        register(0xe4, "CPX", AddressingMode::ZeroPage, 2, 3, 0, CPU::cpx);
        register(0xec, "CPX", AddressingMode::Absolute, 3, 4, 0, CPU::cpx);

        // CPY
        register(0xc0, "CPY", AddressingMode::Immediate, 2, 2, 0, CPU::cpy);
        register(0xc4, "CPY", AddressingMode::ZeroPage, 2, 3, 0, CPU::cpy);
        register(0xcc, "CPY", AddressingMode::Absolute, 3, 4, 0, CPU::cpy);

        // INC
        register(0xe6, "INC", AddressingMode::ZeroPage, 2, 5, 0, CPU::inc);
        register(0xf6, "INC", AddressingMode::ZeroPage_X, 2, 6, 0, CPU::inc);
        register(0xee, "INC", AddressingMode::Absolute, 3, 6, 0, CPU::inc);
        register(0xfe, "INC", AddressingMode::Absolute_X, 3, 7, 0, CPU::inc);

        // DEC
        register(0xc6, "DEC", AddressingMode::ZeroPage, 2, 5, 0, CPU::dec);
        register(0xd6, "DEC", AddressingMode::ZeroPage_X, 2, 6, 0, CPU::dec);
        register(0xce, "DEC", AddressingMode::Absolute, 3, 6, 0, CPU::dec);
        register(0xde, "DEC", AddressingMode::Absolute_X, 3, 7, 0, CPU::dec);

        // INX
        register(0xe8, "INX", AddressingMode::Implicit, 1, 2, 0, CPU::inx);
        // DEX
        register(0xca, "DEX", AddressingMode::Implicit, 1, 2, 0, CPU::dex);
        // INY
        register(0xc8, "INY", AddressingMode::Implicit, 1, 2, 0, CPU::iny);
        // DEY
        register(0x88, "DEY", AddressingMode::Implicit, 1, 2, 0, CPU::dey);

        // ASL
        register(0x0a, "ASL", AddressingMode::Accumulator, 1, 2, 0, CPU::asl_a);
        register(0x06, "ASL", AddressingMode::ZeroPage, 2, 5, 0, CPU::asl);
        register(0x16, "ASL", AddressingMode::ZeroPage_X, 2, 6, 0, CPU::asl);
        register(0x0e, "ASL", AddressingMode::Absolute, 3, 6, 0, CPU::asl);
        register(0x1e, "ASL", AddressingMode::Absolute_X, 3, 7, 0, CPU::asl);

        // LSR
        register(0x4a, "LSR", AddressingMode::Accumulator, 1, 2, 0, CPU::lsr_a);
        register(0x46, "LSR", AddressingMode::ZeroPage, 2, 5, 0, CPU::lsr);
        register(0x56, "LSR", AddressingMode::ZeroPage_X, 2, 6, 0, CPU::lsr);
        register(0x4e, "LSR", AddressingMode::Absolute, 3, 6, 0, CPU::lsr);
        register(0x5e, "LSR", AddressingMode::Absolute_X, 3, 7, 0, CPU::lsr);

        // ROL
        register(0x2a, "ROL", AddressingMode::Accumulator, 1, 2, 0, CPU::rol_a);
        register(0x26, "ROL", AddressingMode::ZeroPage, 2, 5, 0, CPU::rol);
        register(0x36, "ROL", AddressingMode::ZeroPage_X, 2, 6, 0, CPU::rol);
        register(0x2e, "ROL", AddressingMode::Absolute, 3, 6, 0, CPU::rol);
        register(0x3e, "ROL", AddressingMode::Absolute_X, 3, 7, 0, CPU::rol);

        // ROR
        register(0x6a, "ROR", AddressingMode::Accumulator, 1, 2, 0, CPU::ror_a);
        register(0x66, "ROR", AddressingMode::ZeroPage, 2, 5, 0, CPU::ror);
        register(0x76, "ROR", AddressingMode::ZeroPage_X, 2, 6, 0, CPU::ror);
        register(0x6e, "ROR", AddressingMode::Absolute, 3, 6, 0, CPU::ror);
        register(0x7e, "ROR", AddressingMode::Absolute_X, 3, 7, 0, CPU::ror);

        // JMP
        register(0x4c, "JMP", AddressingMode::Absolute, 3, 3, 0, CPU::jmp);
        register(0x6c, "JMP", AddressingMode::Indirect, 3, 5, 0, CPU::jmp);

        // JSR
        register(0x20, "JSR", AddressingMode::Absolute, 3, 6, 0, CPU::jsr);
        // RTS
        register(0x60, "RTS", AddressingMode::Implicit, 1, 6, 0, CPU::rts);

        // BCC
        register(0x90, "BCC", AddressingMode::Relative, 2, 2, 3, CPU::bcc);
        // BCS
        register(0xb0, "BCS", AddressingMode::Relative, 2, 2, 3, CPU::bcs);
        // BEQ
        register(0xf0, "BEQ", AddressingMode::Relative, 2, 2, 3, CPU::beq);
        // BNE
        register(0xd0, "BNE", AddressingMode::Relative, 2, 2, 3, CPU::bne);
        // BMI
        register(0x30, "BMI", AddressingMode::Relative, 2, 2, 3, CPU::bmi);
        // BPL
        register(0x10, "BPL", AddressingMode::Relative, 2, 2, 3, CPU::bpl);
        // BVC
        register(0x50, "BVC", AddressingMode::Relative, 2, 2, 3, CPU::bvc);
        // BVS
        register(0x70, "BVS", AddressingMode::Relative, 2, 2, 3, CPU::bvs);

        // CLC
        register(0x18, "CLC", AddressingMode::Implicit, 1, 2, 0, CPU::clc);
        // CLD
        register(0xd8, "CLD", AddressingMode::Implicit, 1, 2, 0, CPU::cld);
        // CLI
        register(0x58, "CLI", AddressingMode::Implicit, 1, 2, 0, CPU::cli);
        // CLV
        register(0xb8, "CLV", AddressingMode::Implicit, 1, 2, 0, CPU::clv);
        // SEC
        register(0x38, "SEC", AddressingMode::Implicit, 1, 2, 0, CPU::sec);
        // SED
        register(0xf8, "SED", AddressingMode::Implicit, 1, 2, 0, CPU::sed);
        // SEI
        register(0x78, "SEI", AddressingMode::Implicit, 1, 2, 0, CPU::sei);

        // BRK
        register(0x00, "BRK", AddressingMode::Implicit, 1, 7, 0, CPU::brk);
        // RTI
        register(0x40, "RTI", AddressingMode::Implicit, 1, 6, 0, CPU::rti);

         // NOP
        register(0xea, "NOP", AddressingMode::Implicit, 1, 2, 0, CPU::nop);


        instructions
    };
}

impl CPU {
    #[inline(always)]
    pub fn push_stack(&mut self, val: u8) -> Result<(), Box<dyn Error>> {
        let addr = self.registers.S as Ptr | 0x100;
        self.bus.write(addr, val)?;
        self.registers.S = self.registers.S.wrapping_sub(1);
        Ok(())
    }
    #[inline(always)]
    pub fn push_stack_u16(&mut self, val: u16) -> Result<(), Box<dyn Error>> {
        let low = (val & 0xff) as u8;
        let high = (val >> 8) as u8;
        self.push_stack(high)?;
        self.push_stack(low)
    }
    #[inline(always)]
    pub fn pop_stack(&mut self) -> Result<u8, Box<dyn Error>> {
        self.registers.S = self.registers.S.wrapping_add(1);
        let addr = self.registers.S as Ptr | 0x100;
        self.bus.read(addr)
    }
    #[inline(always)]
    pub fn pop_stack_u16(&mut self) -> Result<u16, Box<dyn Error>> {
        let low = self.pop_stack()?;
        let high = self.pop_stack()?;
        let val = ((high as u16) << 8) | (low as u16);
        Ok(val)
    }
    #[inline(always)]
    fn set_zero_flag(&mut self, val: u8) {
        if val == 0 {
            self.registers.P.set_flag(ProcessorStatus::FLAG_ZERO)
        } else {
            self.registers.P.clear_flag(ProcessorStatus::FLAG_ZERO)
        }
    }
    #[inline(always)]
    fn set_negative_flag(&mut self, val: u8) {
        if val & 0x80 != 0 {
            self.registers.P.set_flag(ProcessorStatus::FLAG_NEGATIVE)
        } else {
            self.registers.P.clear_flag(ProcessorStatus::FLAG_NEGATIVE)
        }
    }
    fn lda(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        self.registers.A = self.bus.read(addr)?;
        self.set_zero_flag(self.registers.A);
        self.set_negative_flag(self.registers.A);
        Ok(())
    }
    fn ldx(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        self.registers.X = self.bus.read(addr)?;
        self.set_zero_flag(self.registers.X);
        self.set_negative_flag(self.registers.X);
        Ok(())
    }
    fn ldy(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        self.registers.Y = self.bus.read(addr)?;
        self.set_zero_flag(self.registers.Y);
        self.set_negative_flag(self.registers.Y);
        Ok(())
    }
    fn sta(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        self.bus.write(addr, self.registers.A)
    }
    fn stx(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        self.bus.write(addr, self.registers.X)
    }
    fn sty(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        self.bus.write(addr, self.registers.Y)
    }
    fn tax(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        self.registers.X = self.registers.A;
        self.set_zero_flag(self.registers.X);
        self.set_negative_flag(self.registers.X);
        Ok(())
    }
    fn tay(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        self.registers.Y = self.registers.A;
        self.set_zero_flag(self.registers.Y);
        self.set_negative_flag(self.registers.Y);
        Ok(())
    }
    fn tsx(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        self.registers.X = self.registers.S;
        self.set_zero_flag(self.registers.X);
        self.set_negative_flag(self.registers.X);
        Ok(())
    }
    fn txa(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        self.registers.A = self.registers.X;
        self.set_zero_flag(self.registers.A);
        self.set_negative_flag(self.registers.A);
        Ok(())
    }
    fn tya(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        self.registers.A = self.registers.Y;
        self.set_zero_flag(self.registers.A);
        self.set_negative_flag(self.registers.A);
        Ok(())
    }
    fn txs(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        self.registers.S = self.registers.X;
        Ok(())
    }
    fn pha(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        self.push_stack(self.registers.A)
    }
    fn php(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        self.push_stack(self.registers.P.0 | ProcessorStatus::FLAG_BREAK | ProcessorStatus::FLAG_ONE)
    }
    fn pla(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        self.registers.A = self.pop_stack()?;
        self.set_zero_flag(self.registers.A);
        self.set_negative_flag(self.registers.A);
        Ok(())
    }
    fn plp(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        let val = ProcessorStatus(self.pop_stack()?);
        // Two instructions (PLP and RTI) pull a byte from the stack and set all the flags. They ignore bits 5 and 4.
        // https://wiki.nesdev.com/w/index.php/Status_flags
        self.registers.P = ProcessorStatus(val.0 & !ProcessorStatus::FLAG_BREAK | ProcessorStatus::FLAG_ONE);
        Ok(())
    }
    fn and(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        let operand = self.bus.read(addr)?;
        self.registers.A &= operand;
        self.set_zero_flag(self.registers.A);
        self.set_negative_flag(self.registers.A);
        Ok(())
    }
    fn eor(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        let operand = self.bus.read(addr)?;
        self.registers.A ^= operand;
        self.set_zero_flag(self.registers.A);
        self.set_negative_flag(self.registers.A);
        Ok(())
    }
    fn ora(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        let operand = self.bus.read(addr)?;
        self.registers.A |= operand;
        self.set_zero_flag(self.registers.A);
        self.set_negative_flag(self.registers.A);
        Ok(())
    }
    fn bit(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        let operand = self.bus.read(addr)?;
        let result = self.registers.A & operand;
        self.set_zero_flag(result);
        self.registers.P.set_flag_value(ProcessorStatus::FLAG_OVERFLOW, operand & 0x40 != 0);
        self.set_negative_flag(operand);
        Ok(())
    }
    fn adc(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        let operand = self.bus.read(addr)?;
        let mut result: u16 = (self.registers.A as u16).wrapping_add(operand as u16);
        if self.registers.P.get_flag(ProcessorStatus::FLAG_CARRY) {
            result = result.wrapping_add(1);
        }
        self.registers.P.set_flag_value(ProcessorStatus::FLAG_CARRY, result > 0xff);
        // http://www.6502.org/tutorials/vflag.html#2.4
        self.registers.P.set_flag_value(ProcessorStatus::FLAG_OVERFLOW, (self.registers.A ^ operand) & 0x80 == 0 && (self.registers.A ^ result as u8) & 0x80 != 0);
        self.registers.A = result as u8;
        self.set_zero_flag(self.registers.A);
        self.set_negative_flag(self.registers.A);
        Ok(())
    }
    fn sbc(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        let mut operand = self.bus.read(addr)?;
        let bit_not = !operand;
        let mut result: u16 = (self.registers.A as u16).wrapping_add(bit_not as u16);
        if self.registers.P.get_flag(ProcessorStatus::FLAG_CARRY) {
            result = result.wrapping_add(1);
        }
        self.registers.P.set_flag_value(ProcessorStatus::FLAG_CARRY, result > 0xff);
        // http://www.6502.org/tutorials/vflag.html#2.4
        self.registers.P.set_flag_value(ProcessorStatus::FLAG_OVERFLOW, (self.registers.A ^ bit_not) & 0x80 == 0 && (self.registers.A ^ result as u8) & 0x80 != 0);
        self.registers.A = result as u8;
        self.set_zero_flag(self.registers.A);
        self.set_negative_flag(self.registers.A);
        Ok(())
    }
    fn cmp(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        let operand = self.bus.read(addr)?;
        let result = self.registers.A.wrapping_sub(operand);
        self.registers.P.set_flag_value(ProcessorStatus::FLAG_CARRY, self.registers.A >= operand);
        self.set_zero_flag(result);
        self.set_negative_flag(result);
        Ok(())
    }
    fn cpx(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        let operand = self.bus.read(addr)?;
        let result = self.registers.X.wrapping_sub(operand);
        self.registers.P.set_flag_value(ProcessorStatus::FLAG_CARRY, self.registers.X >= operand);
        self.set_zero_flag(result);
        self.set_negative_flag(result);
        Ok(())
    }
    fn cpy(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        let operand = self.bus.read(addr)?;
        let result = self.registers.Y.wrapping_sub(operand);
        self.registers.P.set_flag_value(ProcessorStatus::FLAG_CARRY, self.registers.Y >= operand);
        self.set_zero_flag(result);
        self.set_negative_flag(result);
        Ok(())
    }
    fn inc(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        let operand = self.bus.read(addr)?;
        let result = operand.wrapping_add(1);
        self.set_zero_flag(result);
        self.set_negative_flag(result);
        self.bus.write(addr, result)
    }
    fn dec(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        let operand = self.bus.read(addr)?;
        let result = operand.wrapping_sub(1);
        self.set_zero_flag(result);
        self.set_negative_flag(result);
        self.bus.write(addr, result)
    }
    fn inx(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        self.registers.X = self.registers.X.wrapping_add(1);
        self.set_zero_flag(self.registers.X);
        self.set_negative_flag(self.registers.X);
        Ok(())
    }
    fn dex(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        self.registers.X = self.registers.X.wrapping_sub(1);
        self.set_zero_flag(self.registers.X);
        self.set_negative_flag(self.registers.X);
        Ok(())
    }
    fn iny(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        self.registers.Y = self.registers.Y.wrapping_add(1);
        self.set_zero_flag(self.registers.Y);
        self.set_negative_flag(self.registers.Y);
        Ok(())
    }
    fn dey(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        self.registers.Y = self.registers.Y.wrapping_sub(1);
        self.set_zero_flag(self.registers.Y);
        self.set_negative_flag(self.registers.Y);
        Ok(())
    }
    /// Arithmetic Shift Left
    fn asl(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        let operand = self.bus.read(addr)?;
        let result = operand << 1;
        self.set_zero_flag(result);
        self.set_negative_flag(result);
        self.registers.P.set_flag_value(ProcessorStatus::FLAG_CARRY, operand & 0x80 != 0);
        self.bus.write(addr, result)
    }
    fn asl_a(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        let operand = self.registers.A;
        let result = operand << 1;
        self.set_zero_flag(result);
        self.set_negative_flag(result);
        self.registers.P.set_flag_value(ProcessorStatus::FLAG_CARRY, operand & 0x80 != 0);
        self.registers.A = result;
        Ok(())
    }
    /// Logical Shift Right
    fn lsr(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        let operand = self.bus.read(addr)?;
        let result = operand >> 1;
        self.set_zero_flag(result);
        self.set_negative_flag(result);
        self.registers.P.set_flag_value(ProcessorStatus::FLAG_CARRY, operand & 1 != 0);
        self.bus.write(addr, result)
    }
    fn lsr_a(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        let operand = self.registers.A;
        let result = operand >> 1;
        self.set_zero_flag(result);
        self.set_negative_flag(result);
        self.registers.P.set_flag_value(ProcessorStatus::FLAG_CARRY, operand & 1 != 0);
        self.registers.A = result;
        Ok(())
    }
    /// Rotate Left
    fn rol(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        let operand = self.bus.read(addr)?;
        let mut result = operand << 1;
        if self.registers.P.get_flag(ProcessorStatus::FLAG_CARRY) {
            result |= 1;
        }
        self.set_zero_flag(result);
        self.set_negative_flag(result);
        self.registers.P.set_flag_value(ProcessorStatus::FLAG_CARRY, operand & 0x80 != 0);
        self.bus.write(addr, result)
    }
    fn rol_a(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        let operand = self.registers.A;
        let mut result = operand << 1;
        if self.registers.P.get_flag(ProcessorStatus::FLAG_CARRY) {
            result |= 1;
        }
        self.set_zero_flag(result);
        self.set_negative_flag(result);
        self.registers.P.set_flag_value(ProcessorStatus::FLAG_CARRY, operand & 0x80 != 0);
        self.registers.A = result;
        Ok(())
    }
    /// Rotate Right
    fn ror(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        let operand = self.bus.read(addr)?;
        let mut result = operand >> 1;
        if self.registers.P.get_flag(ProcessorStatus::FLAG_CARRY) {
            result |= 0x80;
        }
        self.set_zero_flag(result);
        self.set_negative_flag(result);
        self.registers.P.set_flag_value(ProcessorStatus::FLAG_CARRY, operand & 1 != 0);
        self.bus.write(addr, result)
    }
    fn ror_a(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        let operand = self.registers.A;
        let mut result = operand >> 1;
        if self.registers.P.get_flag(ProcessorStatus::FLAG_CARRY) {
            result |= 0x80;
        }
        self.set_zero_flag(result);
        self.set_negative_flag(result);
        self.registers.P.set_flag_value(ProcessorStatus::FLAG_CARRY, operand & 1 != 0);
        self.registers.A = result;
        Ok(())
    }
    /// Jump
    fn jmp(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        self.registers.PC = addr;
        Ok(())
    }
    /// Jump to Subroutine
    fn jsr(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        self.push_stack_u16(self.registers.PC.wrapping_sub(1))?;
        self.registers.PC = addr;
        Ok(())
    }
    fn rts(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        self.registers.PC = self.pop_stack_u16()?.wrapping_add(1);
        Ok(())
    }
    fn bcs(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        if self.registers.P.get_flag(ProcessorStatus::FLAG_CARRY) {
            self.registers.PC = addr;
        }
        Ok(())
    }
    fn bcc(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        if !self.registers.P.get_flag(ProcessorStatus::FLAG_CARRY) {
            self.registers.PC = addr;
        }
        Ok(())
    }
    fn beq(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        if self.registers.P.get_flag(ProcessorStatus::FLAG_ZERO) {
            self.registers.PC = addr;
        }
        Ok(())
    }
    fn bne(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        if !self.registers.P.get_flag(ProcessorStatus::FLAG_ZERO) {
            self.registers.PC = addr;
        }
        Ok(())
    }
    /// Branch if Minus
    fn bmi(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        if self.registers.P.get_flag(ProcessorStatus::FLAG_NEGATIVE) {
            self.registers.PC = addr;
        }
        Ok(())
    }
    /// Branch if Positive
    fn bpl(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        if !self.registers.P.get_flag(ProcessorStatus::FLAG_NEGATIVE) {
            self.registers.PC = addr;
        }
        Ok(())
    }
    /// Branch if Overflow Set
    fn bvs(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        if self.registers.P.get_flag(ProcessorStatus::FLAG_OVERFLOW) {
            self.registers.PC = addr;
        }
        Ok(())
    }
    /// Branch if Overflow Clear
    fn bvc(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        if !self.registers.P.get_flag(ProcessorStatus::FLAG_OVERFLOW) {
            self.registers.PC = addr;
        }
        Ok(())
    }
    fn clc(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        self.registers.P.clear_flag(ProcessorStatus::FLAG_CARRY);
        Ok(())
    }
    fn cld(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        self.registers.P.clear_flag(ProcessorStatus::FLAG_DECIMAL_MODE);
        Ok(())
    }
    fn cli(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        self.registers.P.clear_flag(ProcessorStatus::FLAG_INTERRUPT_DISABLE);
        Ok(())
    }
    fn clv(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        self.registers.P.clear_flag(ProcessorStatus::FLAG_OVERFLOW);
        Ok(())
    }
    fn sec(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        self.registers.P.set_flag(ProcessorStatus::FLAG_CARRY);
        Ok(())
    }
    fn sed(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        self.registers.P.set_flag(ProcessorStatus::FLAG_DECIMAL_MODE);
        Ok(())
    }
    fn sei(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        self.registers.P.set_flag(ProcessorStatus::FLAG_INTERRUPT_DISABLE);
        Ok(())
    }
    fn brk(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        // Regardless of what ANY 6502 documentation says, BRK is a 2 byte opcode.
        // The first is #$00, and the second is a padding byte.
        self.push_stack_u16(self.registers.PC.wrapping_add(1 ))?;
        let val = self.registers.P.0 | ProcessorStatus::FLAG_BREAK | ProcessorStatus::FLAG_ONE;
        self.push_stack(val)?;
        self.registers.P.set_flag(ProcessorStatus::FLAG_INTERRUPT_DISABLE);
        self.registers.PC = self.bus.read_u16(Self::IV_IRQ_BRK)?;
        Ok(())
    }
    fn rti(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        let val = ProcessorStatus(self.pop_stack()?);
        // Two instructions (PLP and RTI) pull a byte from the stack and set all the flags. They ignore bits 5 and 4.
        // https://wiki.nesdev.com/w/index.php/Status_flags
        self.registers.P = ProcessorStatus(val.0 | ProcessorStatus::FLAG_ONE);
        self.registers.PC = self.pop_stack_u16()?;
        Ok(())
    }
    fn nop(&mut self, addr: Ptr) -> Result<(), Box<dyn Error>> {
        Ok(())
    }
}
