use sdl2::sys::daddr_t;

const RAM_START: u16 = 0x0000;
const RAM_MIRROR_END: u16 = 0x1fff;

// const PPU_REG_START: u16 = 0x2000;
// const PPU_REG_MIRROR_END: u16 = 0x3fff;

/*
    Layers of memory

    0x0000-0x07ff - 2KB internal RAM
    0x0800-0x1fff - Mirrors of internal RAM
    0x2000-0x2007 - PPU registers
    0x2008-0x3fff - Mirrors of PPU registers
    0x4000-0x4017 - APU and I/O registers
    0x4018-0x401f - Other stuff
    0x4020-0xffff - Cartridge ROM
*/

pub struct Memory {
    ram: [u8; 2048],
}

impl Memory {
    pub fn new() -> Self {
        Memory { ram: [0; 2048] }
    }

    pub fn read_mem_u16(&self, address: u16) -> u16 {
        u16::from_le_bytes([self.read_mem(address), self.read_mem(address.wrapping_add(1))])
    }

    pub fn write_mem_u16(&mut self, address: u16, data: u16) {
        let bytes = data.to_le_bytes();
        self.write_mem(address, bytes[0]);
        self.write_mem(address.wrapping_add(1), bytes[1]);
    }

    pub fn read_mem(&self, address: u16) -> u8 {
        match address {
            RAM_START..=RAM_MIRROR_END => self.ram[(address & 0x7FF) as usize],
            _ => panic!("Error: Unknown Memory Address"),
        }
    }

    pub fn write_mem(&mut self, address: u16, value: u8) {
        match address {
            RAM_START..=RAM_MIRROR_END => self.ram[(address & 0x7FF) as usize] = value,
            _ => panic!("Error: Unknown Memory Address"),
        }
    }
}
