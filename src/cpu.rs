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

const STACK_POINTER: u8 = 0xFD;

pub struct CPU<'a> {
    pc: u16,
    acc: u8,
    reg_x: u8,
    reg_y: u8,
    sp: u8,
    status: StatusFlags,
    cycles: u32,
    memory: Rc<RefCell<Memory<'a>>>,
}

impl<'a> CPU<'a> {
    pub fn new(memory_unit: Rc<RefCell<Memory<'a>>>) -> Self {
        // For nestest, pc must be set to 0xC000

        CPU {
            pc: 0,
            acc: 0,
            reg_x: 0,
            reg_y: 0,
            sp: STACK_POINTER,
            status: StatusFlags::empty(),
            cycles: 0,
            memory: memory_unit,
        }
    }

    pub fn reset(&mut self) {
        let mut status = StatusFlags::empty();
        status.set(StatusFlags::UNUSED, true);
        status.set(StatusFlags::INTERRUPT_DISABLE, true);

        self.status = status;
        self.pc = self.read_mem_u16(0xFFFC);
    }

    fn update_operand_cycles(&mut self, opcode_data: &Instruction, does_branch: bool, old_pc: u16) -> u32 {
        let opcode = &opcode_data.instruction;
        let address_mode = opcode_data.address_mode;

        let mut cycles = if does_branch { 1 } else { 0 };

        match opcode
        {
            Opcode::ADC | Opcode::AND | Opcode::CMP | Opcode::EOR | Opcode::LDA | Opcode::ORA | Opcode::LDY | Opcode::LDX | Opcode::SBC => match address_mode
            {
                AddressMode::AbsoluteX =>
                {
                    let addr = self.read_mem_u16(old_pc);
                    let upd_addr = addr.wrapping_add(self.reg_x as u16);

                    if self.is_new_page(upd_addr, addr)
                    {
                        cycles += 1;
                    }
                }
                AddressMode::AbsoluteY =>
                {
                    let addr = self.read_mem_u16(old_pc);
                    let upd_addr = addr.wrapping_add(self.reg_y as u16);

                    if self.is_new_page(upd_addr, addr)
                    {
                        cycles += 1;
                    }
                }
                AddressMode::IndirectY =>
                {
                    let base = self.read_mem(old_pc);

                    let lo = self.read_mem(base as u16);
                    let hi = self.read_mem((base as u8).wrapping_add(1) as u16);

                    let deref_base = (hi as u16) << 8 | (lo as u16);
                    let deref_addr = deref_base.wrapping_add(self.reg_y as u16);

                    if self.is_new_page(deref_addr, deref_base)
                    {
                        cycles += 1;
                    }
                }
                _ =>
                {}
            },
            Opcode::BCC | Opcode::BCS | Opcode::BEQ | Opcode::BMI | Opcode::BNE | Opcode::BPL | Opcode::BVC | Opcode::BVS =>
            {
                let offset = self.read_mem(old_pc) as i8;
                let addr = (old_pc as i16).wrapping_add(offset as i16) as u16;

                if self.is_new_page(old_pc + 1, addr + 1) && does_branch
                {
                    cycles += 1;
                }
            }
            _ =>
            {}
        }

        cycles
    }

    fn get_operand_addr(&mut self, opcode: &Instruction) -> u16 {
        match opcode.address_mode
        {
            AddressMode::Absolute => self.read_mem_u16(self.pc),
            AddressMode::AbsoluteX =>
            {
                let addr = self.read_mem_u16(self.pc);
                addr.wrapping_add(self.reg_x as u16)
            }
            AddressMode::AbsoluteY =>
            {
                let addr = self.read_mem_u16(self.pc);
                addr.wrapping_add(self.reg_y as u16)
            }
            AddressMode::ZeroPage => self.read_mem(self.pc) as u16,
            AddressMode::ZeroPageX =>
            {
                let addr = self.read_mem(self.pc);
                addr.wrapping_add(self.reg_x) as u16
            }
            AddressMode::ZeroPageY =>
            {
                let addr = self.read_mem(self.pc);
                addr.wrapping_add(self.reg_y) as u16
            }
            AddressMode::Relative =>
            {
                let offset = self.read_mem(self.pc) as i8;
                (self.pc as i16).wrapping_add(offset as i16) as u16
            }
            AddressMode::Indirect =>
            {
                let addr = self.read_mem(self.pc);

                let lo = self.read_mem(addr as u16);
                let hi = self.read_mem(addr.wrapping_add(1) as u16);

                (hi as u16) << 8 | (lo as u16)
            }
            AddressMode::IndirectX =>
            {
                let base = self.read_mem(self.pc);
                let ptr = base.wrapping_add(self.reg_x);

                let lo = self.read_mem(ptr as u16);
                let hi = self.read_mem(ptr.wrapping_add(1) as u16);

                (hi as u16) << 8 | (lo as u16)
            }
            AddressMode::IndirectY =>
            {
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

    fn read_mem(&self, addr: u16) -> u8 {
        self.memory.borrow_mut().read_mem(addr)
    }

    fn write_mem(&mut self, addr: u16, value: u8) {
        self.memory.borrow_mut().write_mem(addr, value);
    }

    /*
       CPU uses Little Endian
       0x1234 is stored as 0x34, 0x12
    */
    fn read_mem_u16(&mut self, addr: u16) -> u16 {
        self.memory.borrow_mut().read_mem_u16(addr)
    }

    fn write_mem_u16(&mut self, addr: u16, data: u16) {
        self.memory.borrow_mut().write_mem_u16(addr, data);
    }

    fn push_stack(&mut self, value: u8) {
        let addr = 0x0100 + self.sp as u16;
        self.memory.borrow_mut().write_mem(addr, value);

        self.sp = self.sp.wrapping_sub(1);
    }

    fn pop_stack(&mut self) -> u8 {
        self.sp = self.sp.wrapping_add(1);

        let addr = 0x0100 + self.sp as u16;
        self.memory.borrow_mut().read_mem(addr)
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

        self.cycles += 2;
        self.memory.borrow_mut().update_ppu_cycles(2);

        self.pc = self.read_mem_u16(0xFFFA);
    }

    pub fn run(&mut self) {
        loop
        {
            if self.memory.borrow_mut().get_nmi() == 1
            {
                self.nmi();
            }

            self.interpret();
        }
    }

    fn print_state(&self, opcode: u8, instruction: &Instruction) {
        println!(
            "{:?} {:?} {:#X} {:#X} {:#X} {:#X} {:#X} {:#X} {}",
            instruction.instruction,
            instruction.address_mode,
            opcode,
            self.pc,
            self.reg_x,
            self.reg_y,
            self.acc,
            self.status.bits(),
            self.cycles
        );
    }

    pub fn interpret(&mut self) {
        let opcode = self.memory.borrow_mut().read_mem(self.pc);
        let instruction = &CPU_OPCODES[opcode as usize];

        self.pc += 1;

        let old_pc = self.pc;
        let mut update_pc: bool = true;
        let mut does_branch = false;

        match instruction.instruction
        {
            Opcode::ADC =>
            {
                let addr = self.get_operand_addr(instruction);
                let operand = self.get_operand(addr, instruction.address_mode);

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
            Opcode::AND =>
            {
                // Performs logical AND on accumulator and memory
                let addr = self.get_operand_addr(instruction);
                self.acc &= self.get_operand(addr, instruction.address_mode);

                self.update_zero_flag(self.acc);
                self.update_negative_flag(self.acc);
            }
            Opcode::ASL =>
            {
                // Shifts left one bit (* 2)
                let addr = self.get_operand_addr(instruction);
                let operand = self.get_operand(addr, instruction.address_mode);
                let result = operand << 1;

                self.update_carry_flag(operand & 0b1000_0000 != 0);
                self.update_zero_flag(result);
                self.update_negative_flag(result);

                self.write_operand(addr, result, instruction.address_mode);
            }
            Opcode::BCC =>
            {
                // If carry is clear, branch
                if !self.status.contains(StatusFlags::CARRY)
                {
                    assert!(instruction.address_mode == AddressMode::Relative);
                    self.pc = self.get_operand_addr(instruction);
                    does_branch = true;
                }
            }
            Opcode::BCS =>
            {
                // If carry is set, branch
                if self.status.contains(StatusFlags::CARRY)
                {
                    assert!(instruction.address_mode == AddressMode::Relative);
                    self.pc = self.get_operand_addr(instruction);
                    does_branch = true;
                }
            }
            Opcode::BEQ =>
            {
                // If zero is set, branch
                if self.status.contains(StatusFlags::ZERO)
                {
                    assert!(instruction.address_mode == AddressMode::Relative);
                    self.pc = self.get_operand_addr(instruction);
                    does_branch = true;
                }
            }
            Opcode::BIT =>
            {
                // Tests accumulator against memory
                let addr = self.get_operand_addr(instruction);
                let value = self.read_mem(addr);

                self.update_negative_flag(value);
                self.update_zero_flag(value & self.acc);
                self.update_overflow_flag((value >> 6) & 1 != 0);
            }
            Opcode::BMI =>
            {
                // If negative is set, branch
                if self.status.contains(StatusFlags::NEGATIVE)
                {
                    assert!(instruction.address_mode == AddressMode::Relative);
                    self.pc = self.get_operand_addr(instruction);
                    does_branch = true;
                }
            }
            Opcode::BNE =>
            {
                // If zero is not set, branch
                if !self.status.contains(StatusFlags::ZERO)
                {
                    assert!(instruction.address_mode == AddressMode::Relative);
                    self.pc = self.get_operand_addr(instruction);
                    does_branch = true;
                }
            }
            Opcode::BPL =>
            {
                // If negative is not set, branch
                if !self.status.contains(StatusFlags::NEGATIVE)
                {
                    assert!(instruction.address_mode == AddressMode::Relative);
                    self.pc = self.get_operand_addr(instruction);
                    does_branch = true;
                }
            }
            Opcode::BRK =>
            {
                self.nmi();
                update_pc = false;
            }
            Opcode::BVC =>
            {
                // If overflow is clear, branch
                if !self.status.contains(StatusFlags::OVERFLOW)
                {
                    assert!(instruction.address_mode == AddressMode::Relative);
                    self.pc = self.get_operand_addr(instruction);
                    does_branch = true;
                }
            }
            Opcode::BVS =>
            {
                // If overflow is set, branch
                if self.status.contains(StatusFlags::OVERFLOW)
                {
                    assert!(instruction.address_mode == AddressMode::Relative);
                    self.pc = self.get_operand_addr(instruction);
                    does_branch = true;
                }
            }
            Opcode::CLC =>
            {
                // Clears carry
                self.status.remove(StatusFlags::CARRY);
            }
            Opcode::CLD =>
            {
                // Clears decimal
                self.status.remove(StatusFlags::DECIMAL);
            }
            Opcode::CLI =>
            {
                // Clears interrupt disable
                self.status.remove(StatusFlags::INTERRUPT_DISABLE);
            }
            Opcode::CLV =>
            {
                // Clears overflow
                self.status.remove(StatusFlags::OVERFLOW);
            }
            Opcode::CMP =>
            {
                // Compares accumulator to memory
                let addr = self.get_operand_addr(instruction);
                let operand = self.get_operand(addr, instruction.address_mode);

                self.update_carry_flag(self.acc >= operand);
                self.update_zero_flag((self.acc.wrapping_sub(operand)) as u8);
                self.update_negative_flag((self.acc.wrapping_sub(operand)) as u8);
            }
            Opcode::CPX =>
            {
                // Compares x to memory
                let addr = self.get_operand_addr(instruction);
                let operand = self.get_operand(addr, instruction.address_mode);

                self.update_carry_flag(self.reg_x >= operand);
                self.update_zero_flag((self.reg_x.wrapping_sub(operand)) as u8);
                self.update_negative_flag((self.reg_x.wrapping_sub(operand)) as u8);
            }
            Opcode::CPY =>
            {
                // Compares y to memory
                let addr = self.get_operand_addr(instruction);
                let operand = self.get_operand(addr, instruction.address_mode);

                self.update_carry_flag(self.reg_y >= operand);
                self.update_zero_flag((self.reg_y.wrapping_sub(operand)) as u8);
                self.update_negative_flag((self.reg_y.wrapping_sub(operand)) as u8);
            }
            Opcode::DEC =>
            {
                // Decrement memory
                let addr = self.get_operand_addr(instruction);
                let value = self.get_operand(addr, instruction.address_mode).wrapping_sub(1);
                self.write_mem(addr, value);

                self.update_negative_flag(value);
                self.update_zero_flag(value);
            }
            Opcode::DEX =>
            {
                // Decrement x
                self.reg_x = self.reg_x.wrapping_sub(1);
                self.update_negative_flag(self.reg_x);
                self.update_zero_flag(self.reg_x);
            }
            Opcode::DEY =>
            {
                // Decrement y
                self.reg_y = self.reg_y.wrapping_sub(1);
                self.update_negative_flag(self.reg_y);
                self.update_zero_flag(self.reg_y);
            }
            Opcode::EOR =>
            {
                // Performs exclusive or on accumulator and memory
                let addr = self.get_operand_addr(instruction);
                self.acc ^= self.get_operand(addr, instruction.address_mode);

                self.update_negative_flag(self.acc);
                self.update_zero_flag(self.acc);
            }
            Opcode::INC =>
            {
                // Increment memory
                let addr = self.get_operand_addr(instruction);
                let value = self.get_operand(addr, instruction.address_mode).wrapping_add(1);
                self.write_mem(addr, value);

                self.update_negative_flag(value);
                self.update_zero_flag(value);
            }
            Opcode::INX =>
            {
                // Increment x
                self.reg_x = self.reg_x.wrapping_add(1);
                self.update_negative_flag(self.reg_x);
                self.update_zero_flag(self.reg_x);
            }
            Opcode::INY =>
            {
                // Increment y
                self.reg_y = self.reg_y.wrapping_add(1);
                self.update_negative_flag(self.reg_y);
                self.update_zero_flag(self.reg_y);
            }
            Opcode::JMP =>
            {
                // Sets program counter to address

                if opcode == 0x6c
                {
                    let mem_addr = self.read_mem_u16(self.pc);

                    let indirect_ref = if mem_addr & 0x00FF == 0x00FF
                    {
                        let lo = self.read_mem(mem_addr);
                        let hi = self.read_mem(mem_addr & 0xFF00);
                        (hi as u16) << 8 | (lo as u16)
                    }
                    else
                    {
                        self.read_mem_u16(mem_addr)
                    };

                    self.pc = indirect_ref;
                }
                else
                {
                    self.pc = self.get_operand_addr(instruction);
                }

                update_pc = false;
            }
            Opcode::JSR =>
            {
                // Pushes current program counter to stack and sets program counter to address
                assert!(instruction.address_mode == AddressMode::Absolute);

                let addr_to_push = self.pc + 1;

                self.push_stack((addr_to_push >> 8) as u8);
                self.push_stack((addr_to_push & 0xFF) as u8);

                let addr = self.get_operand_addr(instruction);

                self.pc = addr;

                update_pc = false;
            }
            Opcode::LDA =>
            {
                // Loads byte of memory into accumulator
                let mut addr = self.get_operand_addr(instruction);

                self.acc = self.get_operand(addr, instruction.address_mode);

                self.update_negative_flag(self.acc);
                self.update_zero_flag(self.acc);
            }
            Opcode::LDX =>
            {
                // Loads byte of memory into x
                let addr = self.get_operand_addr(instruction);
                self.reg_x = self.get_operand(addr, instruction.address_mode);

                self.update_negative_flag(self.reg_x);
                self.update_zero_flag(self.reg_x);
            }
            Opcode::LDY =>
            {
                // Loads byte of memory into y
                let addr = self.get_operand_addr(instruction);
                self.reg_y = self.get_operand(addr, instruction.address_mode);

                self.update_negative_flag(self.reg_y);
                self.update_zero_flag(self.reg_y);
            }
            Opcode::LSR =>
            {
                let addr = self.get_operand_addr(instruction);
                let operand = self.get_operand(addr, instruction.address_mode);
                let result = operand >> 1;

                self.update_carry_flag(operand & 0b0000_0001 != 0);
                self.update_zero_flag(result);
                self.update_negative_flag(result);

                self.write_operand(addr, result, instruction.address_mode);
            }
            Opcode::NOP =>
            {
                if instruction.address_mode == AddressMode::Absolute
                {
                    let addr = self.get_operand_addr(instruction);
                    let data = self.read_mem(addr);
                }
            }
            Opcode::ORA =>
            {
                let addr = self.get_operand_addr(instruction);
                let operand = self.get_operand(addr, instruction.address_mode);
                let result = self.acc | operand;

                self.update_zero_flag(result);
                self.update_negative_flag(result);

                self.acc = result;
            }
            Opcode::PHA =>
            {
                self.push_stack(self.acc);
            }
            Opcode::PHP =>
            {
                let mut flags = self.status.clone();
                flags.insert(StatusFlags::B_FLAG);
                flags.insert(StatusFlags::UNUSED);

                self.push_stack(flags.bits());
            }
            Opcode::PLA =>
            {
                self.acc = self.pop_stack();

                self.update_zero_flag(self.acc);
                self.update_negative_flag(self.acc);
            }
            Opcode::PLP =>
            {
                let flags_from_stack = StatusFlags::from_bits(self.pop_stack()).unwrap();
                self.status = flags_from_stack;

                self.status.remove(StatusFlags::B_FLAG);
                self.status.insert(StatusFlags::UNUSED);
            }
            Opcode::ROL =>
            {
                let addr = self.get_operand_addr(instruction);
                let operand = self.get_operand(addr, instruction.address_mode);

                let old_carry = self.status.contains(StatusFlags::CARRY) as u8;

                self.update_carry_flag(operand >> 7 == 1);

                let result = old_carry | (operand << 1);

                self.write_operand(addr, result, instruction.address_mode);

                if instruction.address_mode == AddressMode::Accumulator
                {
                    self.update_zero_flag(result);
                }

                self.update_negative_flag(result);
            }
            Opcode::ROR =>
            {
                let addr = self.get_operand_addr(instruction);
                let operand = self.get_operand(addr, instruction.address_mode);

                let carry = self.status.contains(StatusFlags::CARRY) as u8;

                self.update_carry_flag(operand & 1 != 0);

                let result = (operand >> 1) | (carry << 7);

                self.write_operand(addr, result, instruction.address_mode);
                self.update_negative_flag(result);
            }
            Opcode::RTI =>
            {
                self.status = StatusFlags::from_bits(self.pop_stack()).unwrap();

                self.status.remove(StatusFlags::B_FLAG);
                self.status.insert(StatusFlags::UNUSED);

                self.pc = self.pop_stack() as u16 | ((self.pop_stack() as u16) << 8);
                update_pc = false;
            }
            Opcode::RTS =>
            {
                self.pc = (self.pop_stack() as u16 | ((self.pop_stack() as u16) << 8)) + 1;
                update_pc = false;
            }
            Opcode::SBC =>
            {
                let addr = self.get_operand_addr(instruction);
                let operand = self.get_operand(addr, instruction.address_mode);

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
            Opcode::SEC =>
            {
                self.update_carry_flag(true);
            }
            Opcode::SED =>
            {
                self.update_decimal_flag(true);
            }
            Opcode::SEI =>
            {
                self.update_interrupt_flag(true);
            }
            Opcode::STA =>
            {
                let mut addr = self.get_operand_addr(instruction);
                self.write_mem(addr, self.acc);
            }
            Opcode::STX =>
            {
                let addr = self.get_operand_addr(instruction);
                self.write_mem(addr, self.reg_x);
            }
            Opcode::STY =>
            {
                let addr = self.get_operand_addr(instruction);
                self.write_mem(addr, self.reg_y);
            }
            Opcode::TAX =>
            {
                self.reg_x = self.acc;
                self.update_negative_flag(self.reg_x);
                self.update_zero_flag(self.reg_x);
            }
            Opcode::TAY =>
            {
                self.reg_y = self.acc;
                self.update_negative_flag(self.reg_y);
                self.update_zero_flag(self.reg_y);
            }
            Opcode::TSX =>
            {
                self.reg_x = self.sp;
                self.update_negative_flag(self.reg_x);
                self.update_zero_flag(self.reg_x);
            }
            Opcode::TXA =>
            {
                self.acc = self.reg_x;
                self.update_negative_flag(self.acc);
                self.update_zero_flag(self.acc);
            }
            Opcode::TXS =>
            {
                self.sp = self.reg_x;
            }
            Opcode::TYA =>
            {
                self.acc = self.reg_y;
                self.update_negative_flag(self.acc);
                self.update_zero_flag(self.acc);
            }
            Opcode::SLO =>
            {
                let addr = self.get_operand_addr(instruction);
                let operand = self.get_operand(addr, instruction.address_mode);
                let mut result = operand << 1;

                self.write_operand(addr, result, instruction.address_mode);

                result |= self.acc;

                self.update_carry_flag(operand & 0b1000_0000 != 0);
                self.update_zero_flag(result);
                self.update_negative_flag(result);

                self.acc = result;
            }
            Opcode::RLA =>
            {
                let addr = self.get_operand_addr(instruction);
                let operand = self.get_operand(addr, instruction.address_mode);

                let carry = self.status.contains(StatusFlags::CARRY) as u8;

                self.update_carry_flag(operand >> 7 == 1);

                let mut result = carry | (operand << 1);

                self.write_operand(addr, result, instruction.address_mode);

                self.update_negative_flag(result);

                result = self.acc & result;

                self.acc = result;
            }
            Opcode::SRE =>
            {
                let addr = self.get_operand_addr(instruction);
                let operand = self.get_operand(addr, instruction.address_mode);
                let mut result = operand >> 1;

                self.write_operand(addr, result, instruction.address_mode);

                result ^= self.acc;

                self.update_carry_flag(operand & 0b0000_0001 != 0);
                self.update_zero_flag(result);
                self.update_negative_flag(result);

                self.acc = result;
            }
            Opcode::RRA =>
            {
                let addr = self.get_operand_addr(instruction);
                let operand = self.get_operand(addr, instruction.address_mode);

                let carry = self.status.contains(StatusFlags::CARRY) as u8;

                let ror_result = (operand >> 1) | (carry << 7);

                self.update_carry_flag(operand & 0b0000_0001 != 0);

                self.write_operand(addr, ror_result, instruction.address_mode);

                let carry_in = self.status.contains(StatusFlags::CARRY) as u8;
                let sum = self.acc as u16 + ror_result as u16 + carry_in as u16;
                let result = sum as u8;

                self.update_carry_flag(sum > 0xFF);
                self.update_zero_flag(result);
                self.update_negative_flag(result);

                // Overflow flag is set if the sign bit is incorrect (only relevant in signed arithmetic)
                self.update_overflow_flag(((self.acc ^ result) & (ror_result ^ result) & 0x80) != 0);

                self.acc = result;
            }
            Opcode::SAX =>
            {
                let addr = self.get_operand_addr(instruction);
                self.write_mem(addr, self.reg_x & self.acc);
            }
            Opcode::LAX =>
            {
                let addr = self.get_operand_addr(instruction);
                self.acc = self.get_operand(addr, instruction.address_mode);

                self.reg_x = self.acc;
                self.update_negative_flag(self.reg_x);
                self.update_zero_flag(self.reg_x);
            }
            Opcode::DCP =>
            {
                let addr = self.get_operand_addr(instruction);
                let value = self.get_operand(addr, instruction.address_mode).wrapping_sub(1);

                self.write_mem(addr, value);

                self.update_carry_flag(self.acc >= value);
                self.update_zero_flag((self.acc as i8 - value as i8) as u8);
                self.update_negative_flag((self.acc as i8 - value as i8) as u8);
            }
            Opcode::ISC =>
            {
                let addr = self.get_operand_addr(instruction);
                let inc_value = self.get_operand(addr, instruction.address_mode).wrapping_add(1);

                self.write_mem(addr, inc_value);

                let carry_in = self.status.contains(StatusFlags::CARRY) as u8;
                let value = inc_value ^ 0xFF;
                let sum = self.acc as u16 + value as u16 + carry_in as u16;

                self.update_carry_flag(sum > 0xFF);
                let result = sum as u8;
                self.update_zero_flag(result);
                self.update_negative_flag(result);

                self.update_overflow_flag(((self.acc ^ result) & (self.acc ^ inc_value) & 0x80) != 0);

                self.acc = result;
            }
            Opcode::SKB =>
            {}
            _ =>
            {
                panic!("Error: Unknown opcode");
            }
        }

        let updated_cycles = self.update_operand_cycles(instruction, does_branch, old_pc) + instruction.cycles as u32;

        if update_pc
        {
            self.pc += (instruction.bytes as u16 - 1);
        }

        self.cycles += updated_cycles;
        self.memory.borrow_mut().update_ppu_cycles(updated_cycles);
    }

    fn get_operand(&mut self, addr: u16, address_mode: AddressMode) -> u8 {
        match address_mode
        {
            AddressMode::Immediate => self.read_mem(addr),
            AddressMode::Accumulator => self.acc,
            _ => self.read_mem(addr),
        }
    }

    fn write_operand(&mut self, addr: u16, value: u8, address_mode: AddressMode) {
        match address_mode
        {
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
        self.status.set(StatusFlags::NEGATIVE, value & 0b10000000 != 0);
    }
}
