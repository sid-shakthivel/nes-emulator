use std::{cell::RefCell, ops::Add, rc::Rc, slice::RSplit};

use crate::{
    memory::Memory,
    opcodes::{AddressMode, Instruction, Opcode, CPU_OPCODES},
    ppu::PPU,
};

use bitflags::bitflags;

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct StatusFlags: u8
    {
        const CARRY                 = 0b00000001;
        const ZERO                  = 0b00000010;
        const INTERRUPT_DISABLE     = 0b00000100;
        const DECIMAL               = 0b00001000;
        const B_FLAG                = 0b00010000;
        const UNUSED                = 0b00100000;
        const OVERFLOW              = 0b01000000;
        const NEGATIVE              = 0b10000000;
    }
}

pub struct CPU<'a> {
    pc: u16,
    acc: u8,
    reg_x: u8,
    reg_y: u8,
    sp: u8,
    status: StatusFlags,
    cycles: u32,
    memory: [u8; 0xFFFF],
    memory_unit: Rc<RefCell<Memory<'a>>>,
}

impl<'a> CPU<'a> {
    pub fn new(memory_unit: Rc<RefCell<Memory<'a>>>) -> Self {
        let mut initial_status = StatusFlags::empty();
        initial_status.set(StatusFlags::UNUSED, true);
        initial_status.set(StatusFlags::INTERRUPT_DISABLE, true);

        /*
            For snake, remove setting status registers
            and pc is 0x600

            For nestest pc is 0xC000
        */

        CPU {
            pc: 0,
            acc: 0,
            reg_x: 0,
            reg_y: 0,
            sp: 0xfd,
            status: initial_status,
            cycles: 0,
            memory: [0; 0xFFFF],
            memory_unit,
        }
    }

    pub fn reset(&mut self) {
        self.pc = self.read_mem_u16(0xFFFC);
    }

    fn update_operand_cycles(
        &mut self,
        opcode_data: &Opcode,
        does_branch: bool,
        old_pc: u16,
    ) -> u32 {
        let opcode = &opcode_data.instruction;
        let address_mode = opcode_data.address_mode;

        let mut cycles = 0;

        if does_branch {
            cycles += 1;
        }

        match opcode {
            Instruction::ADC
            | Instruction::AND
            | Instruction::CMP
            | Instruction::EOR
            | Instruction::LDA
            | Instruction::ORA
            | Instruction::LDY
            | Instruction::LDX
            | Instruction::SBC => match address_mode {
                AddressMode::AbsoluteX => {
                    let addr = self.read_mem_u16(old_pc);
                    let upd_addr = addr.wrapping_add(self.reg_x as u16);

                    if self.is_new_page(upd_addr, addr) {
                        cycles += 2;
                    }
                }
                AddressMode::AbsoluteY => {
                    let addr = self.read_mem_u16(old_pc);
                    let upd_addr = addr.wrapping_add(self.reg_y as u16);

                    if self.is_new_page(upd_addr, addr) {
                        cycles += 2;
                    }
                }
                AddressMode::IndirectY => {
                    let base = self.read_mem(old_pc);

                    let lo = self.read_mem(base as u16);
                    let hi = self.read_mem((base as u8).wrapping_add(1) as u16);

                    let deref_base = (hi as u16) << 8 | (lo as u16);
                    let deref_addr = deref_base.wrapping_add(self.reg_y as u16);

                    if self.is_new_page(deref_addr, deref_base) {
                        cycles += 2;
                    }
                }
                _ => {}
            },
            Instruction::BCC
            | Instruction::BCS
            | Instruction::BEQ
            | Instruction::BMI
            | Instruction::BNE
            | Instruction::BPL
            | Instruction::BVC
            | Instruction::BVS => {
                let offset = self.read_mem(old_pc) as i8;
                let addr = (old_pc as i16).wrapping_add(offset as i16) as u16;

                if self.is_new_page(old_pc, addr) {
                    cycles += 2;
                }
            }
            _ => {}
        }

        cycles
    }

    fn get_operand_addr(&mut self, opcode: &Opcode) -> u16 {
        match opcode.address_mode {
            AddressMode::Absolute => self.read_mem_u16(self.pc),
            AddressMode::AbsoluteX => {
                let addr = self.read_mem_u16(self.pc);
                addr.wrapping_add(self.reg_x as u16)
            }
            AddressMode::AbsoluteY => {
                let addr = self.read_mem_u16(self.pc);
                addr.wrapping_add(self.reg_y as u16)
            }
            AddressMode::ZeroPage => self.read_mem(self.pc) as u16,
            AddressMode::ZeroPageX => {
                let addr = self.read_mem(self.pc);
                addr.wrapping_add(self.reg_x) as u16
            }
            AddressMode::ZeroPageY => {
                let addr = self.read_mem(self.pc);
                addr.wrapping_add(self.reg_y) as u16
            }
            AddressMode::Relative => {
                let offset = self.read_mem(self.pc) as i8;
                (self.pc as i16).wrapping_add(offset as i16) as u16
            }
            AddressMode::Indirect => {
                let addr = self.read_mem(self.pc);

                let lo = self.read_mem(addr as u16);
                let hi = self.read_mem(addr.wrapping_add(1) as u16);

                (hi as u16) << 8 | (lo as u16)
            }
            AddressMode::IndirectX => {
                let base = self.read_mem(self.pc);
                let ptr = base.wrapping_add(self.reg_x);

                let lo = self.read_mem(ptr as u16);
                let hi = self.read_mem(ptr.wrapping_add(1) as u16);

                (hi as u16) << 8 | (lo as u16)
            }
            AddressMode::IndirectY => {
                let base = self.read_mem(self.pc);

                let lo = self.read_mem(base as u16);
                let hi = self.read_mem((base as u8).wrapping_add(1) as u16);

                let deref_base = (hi as u16) << 8 | (lo as u16);

                deref_base.wrapping_add(self.reg_y as u16)
            }
            AddressMode::Immediate => self.pc,
            AddressMode::Accumulator => self.acc as u16,
            _ => panic!("Error: Unknown Addressing Mode"),
        }
    }

    pub fn read_mem(&self, addr: u16) -> u8 {
        self.memory_unit.borrow_mut().read_mem(addr)
    }

    pub fn write_mem(&mut self, addr: u16, value: u8) {
        self.memory_unit.borrow_mut().write_mem(addr, value);
    }

    /*
       CPU uses Little Endian
       0x1234 is stored as 0x34, 0x12
    */
    fn read_mem_u16(&mut self, addr: u16) -> u16 {
        self.memory_unit.borrow_mut().read_mem_u16(addr)
    }

    fn write_mem_u16(&mut self, addr: u16, data: u16) {
        self.memory_unit.borrow_mut().write_mem_u16(addr, data);
    }

    fn push_stack(&mut self, value: u8) {
        let addr = 0x0100 + self.sp as u16;
        // self.memory[addr as usize] = value;
        self.memory_unit.borrow_mut().write_mem(addr, value);

        self.sp = self.sp.wrapping_sub(1);
    }

    fn pop_stack(&mut self) -> u8 {
        self.sp = self.sp.wrapping_add(1);

        let addr = 0x0100 + self.sp as u16;
        // self.memory[addr as usize]
        self.memory_unit.borrow_mut().read_mem(addr)
    }

    pub fn load_6502_program(&mut self, program: Vec<u8>) {
        self.memory[0x0600..(0x0600 + program.len())].copy_from_slice(&program[..]);
        self.write_mem_u16(0xFFFC, 0x0600);
    }

    fn is_new_page(&self, addr_a: u16, addr_b: u16) -> bool {
        (addr_a & 0xFF00) != (addr_b & 0xFF00)
    }

    fn nmi(&mut self) {
        self.push_stack((self.pc >> 8) as u8);
        self.push_stack(self.pc as u8);

        let mut flags = self.status.clone();

        flags.set(StatusFlags::B_FLAG, false);
        flags.set(StatusFlags::UNUSED, true);

        self.push_stack(flags.bits());

        self.update_interrupt_flag(true);

        // self.pc = (self.read_mem(0xFFFF) as u16) << 8 | (self.read_mem(0xFFFE) as u16);
        self.cycles += 2;
        self.memory_unit.borrow_mut().update_ppu_cycles(2);

        self.pc = self.read_mem_u16(0xFFFA);
    }

    pub fn run(&mut self) {
        loop {
            if self.memory_unit.borrow_mut().get_nmi() == 1 {
                self.nmi();
            }

            self.interpret();
        }
    }

    pub fn interpret(&mut self) {
        let opcode = self.memory_unit.borrow_mut().read_mem(self.pc);

        let opcode_data = &CPU_OPCODES[opcode as usize];

        println!(
            "{:?} {:?} {:#X} {:#X} {:#X} {:#X} {:#X} {:#X} {}",
            opcode_data.instruction,
            opcode_data.address_mode,
            opcode,
            self.pc,
            self.reg_x,
            self.reg_y,
            self.acc,
            self.status.bits(),
            self.cycles
        );

        // if self.cycles >= 119120 {
        //     panic!("have reached a point of difference");
        // }

        self.pc += 1;

        let old_pc = self.pc;
        let mut update_pc: bool = true;
        let mut does_branch = false;

        match opcode_data.instruction {
            Instruction::ADC => {
                let addr = self.get_operand_addr(opcode_data);
                let operand = self.get_operand(addr, opcode_data.address_mode);

                let carry_in = self.status.contains(StatusFlags::CARRY) as u8;
                let sum = self.acc as u16 + operand as u16 + carry_in as u16;
                let result = sum as u8;

                self.update_carry_flag(sum > 0xFF);
                self.update_zero_flag(result);
                self.update_negative_flag(result);

                // Overflow flag is set if the sign bit is incorrect (only relevant in signed arithmetic)
                self.update_overflow_flag(((self.acc ^ result) & (operand ^ result) & 0x80) != 0);

                self.acc = result;
            }
            Instruction::AND => {
                // Performs logical AND on accumulator and memory
                let addr = self.get_operand_addr(opcode_data);
                self.acc &= self.get_operand(addr, opcode_data.address_mode);

                self.update_zero_flag(self.acc);
                self.update_negative_flag(self.acc);
            }
            Instruction::ASL => {
                // Shifts left one bit (* 2)
                let addr = self.get_operand_addr(opcode_data);
                let operand = self.get_operand(addr, opcode_data.address_mode);
                let result = operand << 1;

                self.update_carry_flag(operand & 0b1000_0000 != 0);
                self.update_zero_flag(result);
                self.update_negative_flag(result);

                self.write_operand(addr, result, opcode_data.address_mode);
            }
            Instruction::BCC => {
                // If carry is clear, branch
                if !self.status.contains(StatusFlags::CARRY) {
                    assert!(opcode_data.address_mode == AddressMode::Relative);
                    self.pc = self.get_operand_addr(opcode_data);
                    does_branch = true;
                }
            }
            Instruction::BCS => {
                // If carry is set, branch
                if self.status.contains(StatusFlags::CARRY) {
                    assert!(opcode_data.address_mode == AddressMode::Relative);
                    self.pc = self.get_operand_addr(opcode_data);
                    does_branch = true;
                }
            }
            Instruction::BEQ => {
                // If zero is set, branch
                if self.status.contains(StatusFlags::ZERO) {
                    assert!(opcode_data.address_mode == AddressMode::Relative);
                    self.pc = self.get_operand_addr(opcode_data);
                    does_branch = true;
                }
            }
            Instruction::BIT => {
                // Tests accumulator against memory
                let addr = self.get_operand_addr(opcode_data);
                let value = self.read_mem(addr);

                self.update_negative_flag(value);
                self.update_zero_flag(value & self.acc);
                self.update_overflow_flag((value >> 6) & 1 != 0);
            }
            Instruction::BMI => {
                // If negative is set, branch
                if self.status.contains(StatusFlags::NEGATIVE) {
                    assert!(opcode_data.address_mode == AddressMode::Relative);
                    self.pc = self.get_operand_addr(opcode_data);
                    does_branch = true;
                }
            }
            Instruction::BNE => {
                // If zero is not set, branch
                if !self.status.contains(StatusFlags::ZERO) {
                    assert!(opcode_data.address_mode == AddressMode::Relative);
                    self.pc = self.get_operand_addr(opcode_data);
                    does_branch = true;
                }
            }
            Instruction::BPL => {
                // If negative is not set, branch
                if !self.status.contains(StatusFlags::NEGATIVE) {
                    assert!(opcode_data.address_mode == AddressMode::Relative);
                    self.pc = self.get_operand_addr(opcode_data);
                    does_branch = true;
                }
            }
            Instruction::BRK => {
                // self.pc += 2;

                // self.push_stack((self.pc >> 8) as u8); // Push high byte of PC
                // self.push_stack(self.pc as u8); // Push low byte of PC

                // self.status.set(StatusFlags::B_FLAG, true);
                // self.push_stack(self.status.bits());
                // self.update_interrupt_flag(true);

                // self.pc = (self.read_mem(0xFFFF) as u16) << 8 | (self.read_mem(0xFFFE) as u16);
                // println!("reached break {:#X}", self.pc);

                std::process::exit(0);
            }
            Instruction::BVC => {
                // If overflow is clear, branch
                if !self.status.contains(StatusFlags::OVERFLOW) {
                    assert!(opcode_data.address_mode == AddressMode::Relative);
                    self.pc = self.get_operand_addr(opcode_data);
                    does_branch = true;
                }
            }
            Instruction::BVS => {
                // If overflow is set, branch
                if self.status.contains(StatusFlags::OVERFLOW) {
                    assert!(opcode_data.address_mode == AddressMode::Relative);
                    self.pc = self.get_operand_addr(opcode_data);
                    does_branch = true;
                }
            }
            Instruction::CLC => {
                // Clears carry
                self.status.remove(StatusFlags::CARRY);
            }
            Instruction::CLD => {
                // Clears decimal
                self.status.remove(StatusFlags::DECIMAL);
            }
            Instruction::CLI => {
                // Clears interrupt disable
                self.status.remove(StatusFlags::INTERRUPT_DISABLE);
            }
            Instruction::CLV => {
                // Clears overflow
                self.status.remove(StatusFlags::OVERFLOW);
            }
            Instruction::CMP => {
                // Compares accumulator to memory
                let addr = self.get_operand_addr(opcode_data);
                let operand = self.get_operand(addr, opcode_data.address_mode);

                self.update_carry_flag(self.acc >= operand);
                self.update_zero_flag((self.acc.wrapping_sub(operand)) as u8);
                self.update_negative_flag((self.acc.wrapping_sub(operand)) as u8);
            }
            Instruction::CPX => {
                // Compares x to memory
                let addr = self.get_operand_addr(opcode_data);
                let operand = self.get_operand(addr, opcode_data.address_mode);

                self.update_carry_flag(self.reg_x >= operand);
                self.update_zero_flag((self.reg_x.wrapping_sub(operand)) as u8);
                self.update_negative_flag((self.reg_x.wrapping_sub(operand)) as u8);
            }
            Instruction::CPY => {
                // Compares y to memory
                let addr = self.get_operand_addr(opcode_data);
                let operand = self.get_operand(addr, opcode_data.address_mode);

                self.update_carry_flag(self.reg_y >= operand);
                self.update_zero_flag((self.reg_y.wrapping_sub(operand)) as u8);
                self.update_negative_flag((self.reg_y.wrapping_sub(operand)) as u8);
            }
            Instruction::DEC => {
                // Decrement memory
                let addr = self.get_operand_addr(opcode_data);
                let value = self
                    .get_operand(addr, opcode_data.address_mode)
                    .wrapping_sub(1);
                self.write_mem(addr, value);

                self.update_negative_flag(value);
                self.update_zero_flag(value);
            }
            Instruction::DEX => {
                // Decrement x
                self.reg_x = self.reg_x.wrapping_sub(1);
                self.update_negative_flag(self.reg_x);
                self.update_zero_flag(self.reg_x);
            }
            Instruction::DEY => {
                // Decrement y
                self.reg_y = self.reg_y.wrapping_sub(1);
                self.update_negative_flag(self.reg_y);
                self.update_zero_flag(self.reg_y);
            }
            Instruction::EOR => {
                // Performs exclusive or on accumulator and memory
                let addr = self.get_operand_addr(opcode_data);
                self.acc ^= self.get_operand(addr, opcode_data.address_mode);

                self.update_negative_flag(self.acc);
                self.update_zero_flag(self.acc);
            }
            Instruction::INC => {
                // Increment memory
                let addr = self.get_operand_addr(opcode_data);
                let value = self
                    .get_operand(addr, opcode_data.address_mode)
                    .wrapping_add(1);
                self.write_mem(addr, value);

                self.update_negative_flag(value);
                self.update_zero_flag(value);
            }
            Instruction::INX => {
                // Increment x
                self.reg_x = self.reg_x.wrapping_add(1);
                self.update_negative_flag(self.reg_x);
                self.update_zero_flag(self.reg_x);
            }
            Instruction::INY => {
                // Increment y
                self.reg_y = self.reg_y.wrapping_add(1);
                self.update_negative_flag(self.reg_y);
                self.update_zero_flag(self.reg_y);
            }
            Instruction::JMP => {
                // Sets program counter to address

                if opcode == 0x6c {
                    let mem_addr = self.read_mem_u16(self.pc);

                    let indirect_ref = if mem_addr & 0x00FF == 0x00FF {
                        let lo = self.read_mem(mem_addr);
                        let hi = self.read_mem(mem_addr & 0xFF00);
                        (hi as u16) << 8 | (lo as u16)
                    } else {
                        self.read_mem_u16(mem_addr)
                    };

                    self.pc = indirect_ref;
                } else {
                    self.pc = self.get_operand_addr(opcode_data);
                }

                update_pc = false;
            }
            Instruction::JSR => {
                // Pushes current program counter to stack and sets program counter to address
                assert!(opcode_data.address_mode == AddressMode::Absolute);

                let addr_to_push = self.pc + 1;

                self.push_stack((addr_to_push >> 8) as u8);
                self.push_stack((addr_to_push & 0xFF) as u8);

                let addr = self.get_operand_addr(opcode_data);

                self.pc = addr;

                update_pc = false;
            }
            Instruction::LDA => {
                // Loads byte of memory into accumulator
                let mut addr = self.get_operand_addr(opcode_data);

                self.acc = self.get_operand(addr, opcode_data.address_mode);

                self.update_negative_flag(self.acc);
                self.update_zero_flag(self.acc);
            }
            Instruction::LDX => {
                // Loads byte of memory into x
                let addr = self.get_operand_addr(opcode_data);
                self.reg_x = self.get_operand(addr, opcode_data.address_mode);

                self.update_negative_flag(self.reg_x);
                self.update_zero_flag(self.reg_x);
            }
            Instruction::LDY => {
                // Loads byte of memory into y
                let addr = self.get_operand_addr(opcode_data);
                self.reg_y = self.get_operand(addr, opcode_data.address_mode);

                self.update_negative_flag(self.reg_y);
                self.update_zero_flag(self.reg_y);
            }
            Instruction::LSR => {
                let addr = self.get_operand_addr(opcode_data);
                let operand = self.get_operand(addr, opcode_data.address_mode);
                let result = operand >> 1;

                self.update_carry_flag(operand & 0b0000_0001 != 0);
                self.update_zero_flag(result);
                self.update_negative_flag(result);

                self.write_operand(addr, result, opcode_data.address_mode);
            }
            Instruction::NOP => {
                if opcode_data.address_mode == AddressMode::Absolute {
                    let addr = self.get_operand_addr(opcode_data);
                    let data = self.read_mem(addr);
                }
            }
            Instruction::ORA => {
                let addr = self.get_operand_addr(opcode_data);
                let operand = self.get_operand(addr, opcode_data.address_mode);
                let result = self.acc | operand;

                self.update_zero_flag(result);
                self.update_negative_flag(result);

                self.acc = result;
            }
            Instruction::PHA => {
                self.push_stack(self.acc);
            }
            Instruction::PHP => {
                let mut flags = self.status.clone();
                flags.insert(StatusFlags::B_FLAG);
                flags.insert(StatusFlags::UNUSED);

                self.push_stack(flags.bits());
            }
            Instruction::PLA => {
                self.acc = self.pop_stack();

                self.update_zero_flag(self.acc);
                self.update_negative_flag(self.acc);
            }
            Instruction::PLP => {
                let flags_from_stack = StatusFlags::from_bits(self.pop_stack()).unwrap();
                self.status = flags_from_stack;

                self.status.remove(StatusFlags::B_FLAG);
                self.status.insert(StatusFlags::UNUSED);
            }
            Instruction::ROL => {
                let addr = self.get_operand_addr(opcode_data);
                let operand = self.get_operand(addr, opcode_data.address_mode);

                let old_carry = self.status.contains(StatusFlags::CARRY) as u8;

                self.update_carry_flag(operand >> 7 == 1);

                let result = old_carry | (operand << 1);

                self.write_operand(addr, result, opcode_data.address_mode);

                self.update_zero_flag(result);
                self.update_negative_flag(result);
            }
            Instruction::ROR => {
                let addr = self.get_operand_addr(opcode_data);
                let operand = self.get_operand(addr, opcode_data.address_mode);

                let carry = self.status.contains(StatusFlags::CARRY) as u8;

                let result = (operand >> 1) | (carry << 7);

                self.write_operand(addr, result, opcode_data.address_mode);

                self.update_carry_flag(operand & 0b0000_0001 != 0);
                self.update_zero_flag(result);
                self.update_negative_flag(result);
            }
            Instruction::RTI => {
                self.status = StatusFlags::from_bits(self.pop_stack()).unwrap();

                self.status.remove(StatusFlags::B_FLAG);
                self.status.insert(StatusFlags::UNUSED);

                self.pc = self.pop_stack() as u16 | ((self.pop_stack() as u16) << 8);
                update_pc = false;
            }
            Instruction::RTS => {
                self.pc = (self.pop_stack() as u16 | ((self.pop_stack() as u16) << 8)) + 1;
                update_pc = false;
            }
            Instruction::SBC => {
                let addr = self.get_operand_addr(opcode_data);
                let operand = self.get_operand(addr, opcode_data.address_mode);

                let carry_in = self.status.contains(StatusFlags::CARRY) as u8;
                let value = operand ^ 0xFF;
                let sum = self.acc as u16 + value as u16 + carry_in as u16;

                self.update_carry_flag(sum > 0xFF);
                let result = sum as u8;
                self.update_zero_flag(result);
                self.update_negative_flag(result);

                self.update_overflow_flag(((self.acc ^ result) & (self.acc ^ operand) & 0x80) != 0);

                self.acc = result;
            }
            Instruction::SEC => {
                self.update_carry_flag(true);
            }
            Instruction::SED => {
                self.update_decimal_flag(true);
            }
            Instruction::SEI => {
                self.update_interrupt_flag(true);
            }
            Instruction::STA => {
                let mut addr = self.get_operand_addr(opcode_data);

                // if self.pc == 0xEE09 {
                //     println!("storing {:#X}", self.acc);
                // }

                self.write_mem(addr, self.acc);
            }
            Instruction::STX => {
                let addr = self.get_operand_addr(opcode_data);
                self.write_mem(addr, self.reg_x);
            }
            Instruction::STY => {
                let addr = self.get_operand_addr(opcode_data);
                self.write_mem(addr, self.reg_y);
            }
            Instruction::TAX => {
                self.reg_x = self.acc;
                self.update_negative_flag(self.reg_x);
                self.update_zero_flag(self.reg_x);
            }
            Instruction::TAY => {
                self.reg_y = self.acc;
                self.update_negative_flag(self.reg_y);
                self.update_zero_flag(self.reg_y);
            }
            Instruction::TSX => {
                self.reg_x = self.sp;
                self.update_negative_flag(self.reg_x);
                self.update_zero_flag(self.reg_x);
            }
            Instruction::TXA => {
                self.acc = self.reg_x;
                self.update_negative_flag(self.acc);
                self.update_zero_flag(self.acc);
            }
            Instruction::TXS => {
                self.sp = self.reg_x;
            }
            Instruction::TYA => {
                self.acc = self.reg_y;
                self.update_negative_flag(self.acc);
                self.update_zero_flag(self.acc);
            }
            Instruction::SLO => {
                let addr = self.get_operand_addr(opcode_data);
                let operand = self.get_operand(addr, opcode_data.address_mode);
                let mut result = operand << 1;

                self.write_operand(addr, result, opcode_data.address_mode);

                result |= self.acc;

                self.update_carry_flag(operand & 0b1000_0000 != 0);
                self.update_zero_flag(result);
                self.update_negative_flag(result);

                self.acc = result;
            }
            Instruction::RLA => {
                let addr = self.get_operand_addr(opcode_data);
                let operand = self.get_operand(addr, opcode_data.address_mode);

                let carry = self.status.contains(StatusFlags::CARRY) as u8;

                self.update_carry_flag(operand >> 7 == 1);

                let mut result = carry | (operand << 1);

                self.write_operand(addr, result, opcode_data.address_mode);

                self.update_negative_flag(result);

                result = self.acc & result;

                self.acc = result;
            }
            Instruction::SRE => {
                let addr = self.get_operand_addr(opcode_data);
                let operand = self.get_operand(addr, opcode_data.address_mode);
                let mut result = operand >> 1;

                self.write_operand(addr, result, opcode_data.address_mode);

                result ^= self.acc;

                self.update_carry_flag(operand & 0b0000_0001 != 0);
                self.update_zero_flag(result);
                self.update_negative_flag(result);

                self.acc = result;
            }
            Instruction::RRA => {
                let addr = self.get_operand_addr(opcode_data);
                let operand = self.get_operand(addr, opcode_data.address_mode);

                let carry = self.status.contains(StatusFlags::CARRY) as u8;

                let ror_result = (operand >> 1) | (carry << 7);

                self.update_carry_flag(operand & 0b0000_0001 != 0);

                self.write_operand(addr, ror_result, opcode_data.address_mode);

                let carry_in = self.status.contains(StatusFlags::CARRY) as u8;
                let sum = self.acc as u16 + ror_result as u16 + carry_in as u16;
                let result = sum as u8;

                self.update_carry_flag(sum > 0xFF);
                self.update_zero_flag(result);
                self.update_negative_flag(result);

                // Overflow flag is set if the sign bit is incorrect (only relevant in signed arithmetic)
                self.update_overflow_flag(
                    ((self.acc ^ result) & (ror_result ^ result) & 0x80) != 0,
                );

                self.acc = result;
            }
            Instruction::SAX => {
                let addr = self.get_operand_addr(opcode_data);
                self.write_mem(addr, self.reg_x & self.acc);
            }
            Instruction::LAX => {
                let addr = self.get_operand_addr(opcode_data);
                self.acc = self.get_operand(addr, opcode_data.address_mode);

                self.reg_x = self.acc;
                self.update_negative_flag(self.reg_x);
                self.update_zero_flag(self.reg_x);
            }
            Instruction::DCP => {
                let addr = self.get_operand_addr(opcode_data);
                let value = self
                    .get_operand(addr, opcode_data.address_mode)
                    .wrapping_sub(1);

                self.write_mem(addr, value);

                self.update_carry_flag(self.acc >= value);
                self.update_zero_flag((self.acc as i8 - value as i8) as u8);
                self.update_negative_flag((self.acc as i8 - value as i8) as u8);
            }
            Instruction::ISC => {
                let addr = self.get_operand_addr(opcode_data);
                let inc_value = self
                    .get_operand(addr, opcode_data.address_mode)
                    .wrapping_add(1);

                self.write_mem(addr, inc_value);

                let carry_in = self.status.contains(StatusFlags::CARRY) as u8;
                let value = inc_value ^ 0xFF;
                let sum = self.acc as u16 + value as u16 + carry_in as u16;

                self.update_carry_flag(sum > 0xFF);
                let result = sum as u8;
                self.update_zero_flag(result);
                self.update_negative_flag(result);

                self.update_overflow_flag(
                    ((self.acc ^ result) & (self.acc ^ inc_value) & 0x80) != 0,
                );

                self.acc = result;
            }
            Instruction::SKB => {}
            _ => {
                panic!("Error: Unknown opcode");
            }
        }

        let updated_cycles = self.update_operand_cycles(opcode_data, does_branch, old_pc)
            + opcode_data.cycles as u32;

        if update_pc {
            self.pc += (opcode_data.bytes as u16 - 1);
        }

        self.cycles += updated_cycles;
        self.memory_unit
            .borrow_mut()
            .update_ppu_cycles(updated_cycles);
    }

    fn get_operand(&mut self, addr: u16, address_mode: AddressMode) -> u8 {
        match address_mode {
            AddressMode::Immediate => self.read_mem(addr),
            AddressMode::Accumulator => self.acc,
            _ => self.read_mem(addr),
        }
    }

    fn write_operand(&mut self, addr: u16, value: u8, address_mode: AddressMode) {
        match address_mode {
            AddressMode::Accumulator => self.acc = value,
            _ => self.write_mem(addr, value),
        }
    }

    fn update_interrupt_flag(&mut self, value: bool) {
        self.status.set(StatusFlags::INTERRUPT_DISABLE, value);
    }

    fn update_decimal_flag(&mut self, value: bool) {
        self.status.set(StatusFlags::DECIMAL, value);
    }

    fn update_overflow_flag(&mut self, value: bool) {
        self.status.set(StatusFlags::OVERFLOW, value);
    }

    fn update_carry_flag(&mut self, value: bool) {
        self.status.set(StatusFlags::CARRY, value);
    }

    fn update_zero_flag(&mut self, value: u8) {
        self.status.set(StatusFlags::ZERO, value == 0);
    }

    fn update_negative_flag(&mut self, value: u8) {
        self.status
            .set(StatusFlags::NEGATIVE, value & 0b1000_0000 != 0);
    }
}
