use std::{cell::RefCell, os::raw, rc::Rc};

use bitflags::bitflags;

use crate::ppu::PPU;

const RAM_START: u16 = 0x0000;
const RAM_MIRROR_END: u16 = 0x1fff;

const PRG_ROM_START: u16 = 0x8000;
const PRG_ROM_END: u16 = 0xffff;

const PPU_REG_START: u16 = 0x2008;
const PPU_REG_MIRROR_END: u16 = 0x3fff;

/*
    Memory Map

    0x0000-0x07ff - 2KB internal RAM
    0x0800-0x1fff - Mirrors of internal RAM
    0x2000-0x2007 - PPU registers
    0x2008-0x3fff - Mirrors of PPU registers
    0x4000-0x4017 - APU and I/O registers
    0x4018-0x401f - Other stuff
    0x4020-0x5fff - Expansion ROM
    0x6000-0x7fff - Save RAM
    0x8000-0xffff - PRG ROM
*/

pub struct Memory {
    ram: [u8; 2048],
    prg_rom: Vec<u8>,
    ppu: Rc<RefCell<PPU>>,
}

impl Memory {
    pub fn new(prg_rom: Vec<u8>, ppu: Rc<RefCell<PPU>>) -> Self {
        Memory {
            ram: [0; 2048],
            prg_rom,
            ppu,
        }
    }

    pub fn read_mem_u16(&self, addr: u16) -> u16 {
        let low = self.read_mem(addr) as u16;
        let high = self.read_mem(addr + 1) as u16;
        (high << 8) | (low as u16)
    }

    pub fn write_mem_u16(&mut self, addr: u16, data: u16) {
        let high = (data >> 8) as u8;
        let low = (data & 0xff) as u8;
        self.write_mem(addr, low);
        self.write_mem(addr + 1, high);
    }

    pub fn read_mem(&self, addr: u16) -> u8 {
        match addr {
            RAM_START..=RAM_MIRROR_END => self.ram[(addr & 0x7FF) as usize],
            0x2000 | 0x2001 | 0x2003 | 0x2005 | 0x2006 | 0x4014 => {
                panic!("Attempt to read from write-only PPU address {:x}", addr);
            }
            0x2002 => self.ppu.borrow_mut().read_status_reg(),
            0x2004 => self.ppu.borrow_mut().read_oam_data(),
            0x2007 => self.ppu.borrow_mut().read(),
            PPU_REG_START..=PPU_REG_MIRROR_END => self.read_mem(addr & 0x2007),
            PRG_ROM_START..=PRG_ROM_END => self.read_prg_rom(addr),
            _ => {
                panic!("Error: Unknown Memory Address {:#X}", addr);
            }
        }
    }

    pub fn write_mem(&mut self, addr: u16, value: u8) {
        match addr {
            RAM_START..=RAM_MIRROR_END => self.ram[(addr) as usize] = value,
            0x2000 => self.ppu.borrow_mut().write_ctrl_reg(value),
            0x2001 => self.ppu.borrow_mut().write_mask_reg(value),
            0x2003 => self.ppu.borrow_mut().write_oam_addr(value),
            0x2004 => self.ppu.borrow_mut().write_oam_data(value),
            0x2005 => panic!("Error: Emulator does not support scrolling"),
            0x2006 => self.ppu.borrow_mut().write_addr_reg(value),
            0x2007 => self.ppu.borrow_mut().write(),
            // 0x4014 => self.ppu.borrow_mut().write_oam_dma(value),
            PPU_REG_START..=PPU_REG_MIRROR_END => self.write_mem(addr & 0x2007, value),
            PRG_ROM_START..=PRG_ROM_END => panic!("Error: PRG_ROM is read only"),
            _ => {
                panic!("Error: Unknown Memory Address {:#X}", addr);
            }
        }
    }

    fn read_prg_rom(&self, mut addr: u16) -> u8 {
        addr -= 0x8000;

        if self.prg_rom.len() == 0x4000 && addr >= 0x4000 {
            addr = addr % 0x4000;
        }

        self.prg_rom[addr as usize]
    }
}
