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
    does_branch: bool,

    memory: Rc<RefCell<Memory<'a>>>,
    ppu: Rc<RefCell<PPU<'a>>>,
}

impl<'a> CPU<'a> {
    pub fn new(memory: Rc<RefCell<Memory<'a>>>, ppu: Rc<RefCell<PPU<'a>>>) -> Self {
        // For nestest, pc must be set to 0xC000

        CPU {
            pc: 0,
            acc: 0,
            reg_x: 0,
            reg_y: 0,
            sp: STACK_POINTER,
            status: StatusFlags::empty(),
            cycles: 0,
            does_branch: false,
            memory,
            ppu,
        }
    }

    pub fn reset(&mut self) {
        let mut status = StatusFlags::empty();
        status.set(StatusFlags::UNUSED, true);
        status.set(StatusFlags::INTERRUPT_DISABLE, true);

        self.status = status;
        self.pc = self.read_mem_u16(0xFFFC);
    }

    fn update_operand_cycles(&mut self, opcode_data: &Instruction, old_pc: u16) -> u32 {
        let opcode = &opcode_data.instruction;
        let address_mode = opcode_data.address_mode;

        let mut cycles = if self.does_branch { 1 } else { 0 };

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

                if self.is_new_page(old_pc + 1, addr + 1) && self.does_branch
                {
                    cycles += 1;
                }
            }
            _ =>
            {}
        }

        cycles
    }

    fn get_operand_addr(&mut self, addr_mode: &AddressMode) -> u16 {
        match addr_mode
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
                let mem_addr = self.read_mem_u16(self.pc);

                if mem_addr & 0x00FF == 0x00FF 
                {
                    let low = self.read_mem(mem_addr as u16);
                    let high = self.read_mem(mem_addr.wrapping_add(1) as u16);
    
                    (high as u16) << 8 | (low as u16)
                } else {
                    self.read_mem_u16(mem_addr)
                }
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

        self.update_cycles(2);

        self.pc = self.read_mem_u16(0xFFFA);
    }

    pub fn run(&mut self) {
        loop
        {
            if self.ppu.borrow_mut().get_nmi()
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

    fn branch(&mut self, addr_mode: &AddressMode, condition: bool) {
        if condition
        {
            assert!(*addr_mode == AddressMode::Relative);
            self.pc = self.get_operand_addr(addr_mode);
            self.does_branch = true;
        }
    }

    fn cmp(&mut self, addr_mode: &AddressMode, cmp_value: u8) {
        let (addr, operand) = self.get_operand(&addr_mode);

        self.update_carry_flag(cmp_value >= operand);
        self.update_zero_flag((cmp_value.wrapping_sub(operand)) as u8);
        self.update_negative_flag((cmp_value.wrapping_sub(operand)) as u8);
    }

    fn ld(&mut self, addr_mode: &AddressMode) -> u8 {
        let (addr, operand) = self.get_operand(&addr_mode);
        self.update_zero_and_negative_flags(operand);
        operand
    }

    fn upd_acc(&mut self, value: u8) {
        self.acc = value;
        self.update_zero_and_negative_flags(self.acc);
    }

    fn upd_reg_x(&mut self, value: u8) {
        self.reg_x = value;
        self.update_zero_and_negative_flags(self.reg_x);
    }

    fn upd_reg_y(&mut self, value: u8) {
        self.reg_y = value;
        self.update_zero_and_negative_flags(self.reg_y);
    }

    fn ror(&mut self, addr_mode: &AddressMode) -> u8
    {
        let (addr, operand) = self.get_operand(&addr_mode);

        let carry = self.status.contains(StatusFlags::CARRY) as u8;
        let result = (operand >> 1) | (carry << 7);

        self.write_operand(addr, result, &addr_mode);
        self.update_carry_flag(operand & 1 != 0);

        result
    }

    fn adc(&mut self, value: u8)
    {
        let carry_in = self.status.contains(StatusFlags::CARRY) as u8;
        let sum = self.acc as u16 + value as u16 + carry_in as u16;
        let result = sum as u8;

        self.update_carry_flag(sum > 0xFF);

        // Overflow flag is set if the sign bit is incorrect (only relevant in signed arithmetic)
        self.update_overflow_flag(((self.acc ^ result) & (value ^ result) & 0x80) != 0);

        self.upd_acc(result);
    }

    fn sbc(&mut self, mut value: u8)
    {
        let carry_in = self.status.contains(StatusFlags::CARRY) as u8;
        value ^= 0xFF;
        let sum = self.acc as u16 + value as u16 + carry_in as u16;
        let result = sum as u8;

        self.update_carry_flag(sum > 0xFF);
        self.update_overflow_flag(((self.acc ^ result) & (self.acc ^ value) & 0x80) != 0);

        self.upd_acc(result);
    }

    fn lsr(&mut self, value: u8, addr: u16, addr_mode: &AddressMode) -> u8
    {
        let result = value >> 1;

        self.update_carry_flag(value & 0b00000001 != 0);
        self.update_zero_and_negative_flags(result);

        self.write_operand(addr, result, &addr_mode);

        result
    }

    fn dec(&mut self, addr: u16, mut value: u8) -> u8
    {
        value = value.wrapping_sub(1);
        self.write_mem(addr, value);
        self.update_zero_and_negative_flags(value);

        value
    }

    fn inc(&mut self, addr: u16, mut value: u8) -> u8 
    {
        value = value.wrapping_add(1);
        self.write_mem(addr, value);
        self.update_zero_and_negative_flags(value);

        value
    }

    fn asl(&mut self, addr_mode: &AddressMode) -> u8
    {
        let (addr, operand) = self.get_operand(&addr_mode);
        let result = operand << 1;

        self.write_operand(addr, result, &addr_mode);

        self.update_carry_flag(operand & 0b10000000 != 0);
        self.update_zero_and_negative_flags(result);

        result
    }

    fn rol(&mut self, addr_mode: &AddressMode) -> u8
    {
        let (addr, operand) = self.get_operand(&addr_mode);
        let old_carry = self.status.contains(StatusFlags::CARRY) as u8;

        let result = old_carry | (operand << 1);

        self.write_operand(addr, result, &addr_mode);

        if *addr_mode == AddressMode::Accumulator
        {
            self.update_zero_flag(result);
        }

        self.update_carry_flag(operand >> 7 == 1);
        self.update_negative_flag(result);

        result
    }

    pub fn interpret(&mut self) {
        let opcode = self.memory.borrow_mut().read_mem(self.pc);
        let instruction = &CPU_OPCODES[opcode as usize];

        self.pc += 1;

        let old_pc = self.pc;
        let mut update_pc: bool = true;

        self.does_branch = false;

        let addr_mode = instruction.address_mode;

        match instruction.instruction
        {
            Opcode::ADC =>
            {
                let (addr, operand) = self.get_operand(&addr_mode);
                self.adc(operand);
            }
            Opcode::AND =>
            {
                let (addr, operand) = self.get_operand(&addr_mode);
                self.upd_acc(self.acc & operand);
            }
            Opcode::ASL => { self.asl(&addr_mode); },
            Opcode::BCC => self.branch(&addr_mode, !self.status.contains(StatusFlags::CARRY)),
            Opcode::BCS => self.branch(&addr_mode, self.status.contains(StatusFlags::CARRY)),
            Opcode::BEQ => self.branch(&addr_mode, self.status.contains(StatusFlags::ZERO)),
            Opcode::BIT =>
            {
                // Tests accumulator against memory
                let addr = self.get_operand_addr(&addr_mode);
                let value = self.read_mem(addr);

                self.update_negative_flag(value);
                self.update_zero_flag(value & self.acc);
                self.update_overflow_flag((value >> 6) & 1 != 0);
            }
            Opcode::BMI => self.branch(&addr_mode, self.status.contains(StatusFlags::NEGATIVE)),
            Opcode::BNE => self.branch(&addr_mode, !self.status.contains(StatusFlags::ZERO)),
            Opcode::BPL => self.branch(&addr_mode, !self.status.contains(StatusFlags::NEGATIVE)),
            Opcode::BRK =>
            {
                self.nmi();
                update_pc = false;
            }
            Opcode::BVC => self.branch(&addr_mode, !self.status.contains(StatusFlags::OVERFLOW)),
            Opcode::BVS => self.branch(&addr_mode, self.status.contains(StatusFlags::OVERFLOW)),
            Opcode::CLC => self.status.remove(StatusFlags::CARRY),
            Opcode::CLD => self.status.remove(StatusFlags::DECIMAL),
            Opcode::CLI => self.status.remove(StatusFlags::INTERRUPT_DISABLE),
            Opcode::CLV => self.status.remove(StatusFlags::OVERFLOW),
            Opcode::CMP => self.cmp(&addr_mode, self.acc),
            Opcode::CPX => self.cmp(&addr_mode, self.reg_x),
            Opcode::CPY => self.cmp(&addr_mode, self.reg_y),
            Opcode::DEC =>
            {
                let (addr, mut operand) = self.get_operand(&addr_mode);
                self.dec(addr, operand);
            }
            Opcode::DEX => self.upd_reg_x(self.reg_x.wrapping_sub(1)),
            Opcode::DEY => self.upd_reg_y(self.reg_y.wrapping_sub(1)),
            Opcode::EOR =>
            {
                let (addr, operand) = self.get_operand(&addr_mode);
                self.upd_acc(self.acc ^ operand);
            }
            Opcode::INC =>
            {
                let (addr, mut operand) = self.get_operand(&addr_mode);
                self.inc(addr, operand);
            }
            Opcode::INX => self.upd_reg_x(self.reg_x.wrapping_add(1)),
            Opcode::INY => self.upd_reg_y(self.reg_y.wrapping_add(1)),
            Opcode::JMP =>
            {
                self.pc = self.get_operand_addr(&addr_mode);
                update_pc = false;
            }
            Opcode::JSR =>
            {
                // Pushes current program counter to stack and sets program counter to address
                assert!(instruction.address_mode == AddressMode::Absolute);

                let addr_to_push = self.pc.wrapping_add(1);

                self.push_stack((addr_to_push >> 8) as u8);
                self.push_stack((addr_to_push & 0xFF) as u8);

                self.pc = self.get_operand_addr(&addr_mode);

                update_pc = false;
            }
            Opcode::LDA => self.acc = self.ld(&addr_mode),
            Opcode::LDX => self.reg_x = self.ld(&addr_mode),
            Opcode::LDY => self.reg_y = self.ld(&addr_mode),
            Opcode::LSR =>
            {
                let (addr, operand) = self.get_operand(&addr_mode);
                self.lsr(operand, addr, &addr_mode);
            }
            Opcode::NOP =>
            {
                if instruction.address_mode == AddressMode::Absolute
                {
                    let addr = self.get_operand_addr(&addr_mode);
                    let data = self.read_mem(addr);
                }
            }
            Opcode::ORA =>
            {
                let (addr, operand) = self.get_operand(&addr_mode);
                self.upd_acc(self.acc | operand);
            }
            Opcode::PHA => self.push_stack(self.acc),
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
                self.update_zero_and_negative_flags(self.acc);
            }
            Opcode::PLP =>
            {
                let flags_from_stack = StatusFlags::from_bits(self.pop_stack()).unwrap();
                self.status = flags_from_stack;

                self.status.remove(StatusFlags::B_FLAG);
                self.status.insert(StatusFlags::UNUSED);
            }
            Opcode::ROL => { self.rol(&addr_mode); },
            Opcode::ROR =>
            {
                let result = self.ror(&addr_mode);
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
                let (addr, operand) = self.get_operand(&addr_mode);
                self.sbc(operand);
            }
            Opcode::SEC => self.update_carry_flag(true),
            Opcode::SED => self.update_decimal_flag(true),
            Opcode::SEI => self.update_interrupt_flag(true),
            Opcode::STA =>
            {
                let addr = self.get_operand_addr(&addr_mode);
                self.write_mem(addr, self.acc);
            }
            Opcode::STX =>
            {
                let addr = self.get_operand_addr(&addr_mode);
                self.write_mem(addr, self.reg_x);
            }
            Opcode::STY =>
            {
                let addr = self.get_operand_addr(&addr_mode);
                self.write_mem(addr, self.reg_y);
            }
            Opcode::TAX => self.upd_reg_x(self.acc),
            Opcode::TAY => self.upd_reg_y(self.acc),
            Opcode::TSX => self.upd_reg_x(self.sp),
            Opcode::TXA => self.upd_acc(self.reg_x),
            Opcode::TXS => self.sp = self.reg_x,
            Opcode::TYA => self.upd_acc(self.reg_y),
            Opcode::SLO =>
            {
                let result = self.asl(&addr_mode);
                self.acc |= result;
                self.update_zero_and_negative_flags(result);
            }
            Opcode::RLA =>
            {
                let result = self.rol(&addr_mode);
                self.acc &= result;
            }
            Opcode::SRE =>
            {
                let (addr, operand) = self.get_operand(&addr_mode);
                let result = self.lsr(operand, addr, &addr_mode);

                self.acc ^= result;
            }
            Opcode::RRA =>
            {
                let ror_result = self.ror(&addr_mode);
                self.adc(ror_result);
            }
            Opcode::SAX =>
            {
                let addr = self.get_operand_addr(&addr_mode);
                self.write_mem(addr, self.reg_x & self.acc);
            }
            Opcode::LAX =>
            {
                let (addr, operand) = self.get_operand(&addr_mode);

                self.upd_acc(operand);
                self.reg_x = self.acc;
            }
            Opcode::DCP =>
            {
                let (addr, mut operand) = self.get_operand(&addr_mode);
                let result = self.dec(addr, operand);

                self.update_carry_flag(self.acc >= result);
                self.update_zero_and_negative_flags((self.acc as i8 - result as i8) as u8);
            }
            Opcode::ISC =>
            {
                let (addr, mut operand) = self.get_operand(&addr_mode);
                let result = self.inc(addr, operand);
                self.sbc(result);
            }
            _ => panic!("Error: Unknown Opcode"),
        }

        let updated_cycles = self.update_operand_cycles(instruction, old_pc) + instruction.cycles as u32;

        if update_pc
        {
            self.pc += (instruction.bytes as u16 - 1);
        }

        self.update_cycles(updated_cycles);
    }

    fn update_cycles(&mut self, cycles: u32) {
        self.cycles += cycles;
        self.ppu.borrow_mut().update_cycles(cycles);
    }

    fn get_operand(&mut self, addr_mode: &AddressMode) -> (u16, u8) {
        let addr = self.get_operand_addr(&addr_mode);

        match addr_mode
        {
            AddressMode::Accumulator => (addr, self.acc),
            _ => (addr, self.read_mem(addr)),
        }
    }

    fn write_operand(&mut self, addr: u16, value: u8, addr_mode: &AddressMode) {
        match addr_mode
        {
            AddressMode::Accumulator => self.acc = value,
            _ => self.write_mem(addr, value),
        }
    }

    fn update_zero_and_negative_flags(&mut self, value: u8) {
        self.update_zero_flag(value);
        self.update_negative_flag(value);
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
