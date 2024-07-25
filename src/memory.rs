use std::os::raw;

use bitflags::bitflags;

const PRG_ROM_PAGE_SIZE: usize = 16384;
const CHR_ROM_PAGE_SIZE: usize = 8192;

const RAM_START: u16 = 0x0000;
const RAM_MIRROR_END: u16 = 0x1fff;

const PRG_ROM_START: u16 = 0x8000;

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
    0x4020-0x5fff - Expansion ROM
    0x6000-0x7fff - Save RAM
    0x8000-0xffff - PRG ROM
*/

pub struct Memory {
    ram: [u8; 2048],
    prg_rom: Vec<u8>,
    chr_rom: Vec<u8>,
}

bitflags! {
    pub struct ControlByte1: u8
    {
        const VERTICAL_MIRRORING      = 0b00000001;
        const HORIZONTAL_MIRRORING    = 0b00000010;
        const TRAINER                 = 0b00000100;
        const FOUR_SCREEN_VRAM        = 0b00001000;
        const LOWER_ROM_MAPPER        = 0b11110000;
    }

    pub struct ControlByte2: u8
    {
        const FORMAT_1_TYPE           = 0b00000011;
        const FORMAT_2_TYPE           = 0b00001100;
        const UPPER_ROM_MAPPER        = 0b11110000;
    }
}

#[derive(Debug)]
pub enum MirroringType {
    Vertical,
    Horizontal,
    FourScreen,
}

struct ROMHeader {
    string: [u8; 4],
    prg_rom_size: u8, // Number of banks (16KB)
    chr_rom_size: u8, // Number of banks (8KB)
    control_byte_1: ControlByte1,
    control_byte_2: ControlByte2,
    prg_ram_size: u8,
    reserved: [u8; 5],
}

impl ROMHeader {
    fn from_vec(vec: &Vec<u8>) -> ROMHeader {
        ROMHeader {
            string: [vec[0], vec[1], vec[2], vec[3]],
            prg_rom_size: vec[4],
            chr_rom_size: vec[5],
            control_byte_1: ControlByte1::from_bits_retain(vec[6]),
            control_byte_2: ControlByte2::from_bits_retain(vec[7]),
            prg_ram_size: vec[8],
            reserved: [vec[9], vec[10], vec[11], vec[12], vec[13]],
        }
    }
}

impl Memory {
    pub fn new(raw_data: &Vec<u8>) -> Self {
        let header = ROMHeader::from_vec(raw_data);

        assert!(header.string == [0x4e, 0x45, 0x53, 0x1a]);

        let mapper = header.control_byte_1.bits() & ControlByte1::LOWER_ROM_MAPPER.bits()
            | (header.control_byte_2.bits() & ControlByte2::UPPER_ROM_MAPPER.bits());

        let test = (raw_data[7] & 0b1111_0000) | (raw_data[6] >> 4);

        // println!("test: {} mapper: {}", test, mapper);

        let should_skip_trainer = header.control_byte_1.contains(ControlByte1::TRAINER);

        // println!(
        //     "{:#X} {:#X}",
        //     header.control_byte_1.bits(),
        //     header.control_byte_2.bits(),
        // );

        assert!(!header.control_byte_2.contains(ControlByte2::FORMAT_1_TYPE));
        assert!(!header.control_byte_2.contains(ControlByte2::FORMAT_2_TYPE));

        let four_screen = header
            .control_byte_1
            .contains(ControlByte1::FOUR_SCREEN_VRAM);
        let vertical_mirroring = header
            .control_byte_1
            .contains(ControlByte1::VERTICAL_MIRRORING);
        let screen_mirroring = match (four_screen, vertical_mirroring) {
            (true, _) => MirroringType::FourScreen,
            (false, true) => MirroringType::Vertical,
            (false, false) => MirroringType::Horizontal,
        };

        // println!("Mirroring: {:?}", screen_mirroring);

        let prg_rom_size = header.prg_rom_size as usize * PRG_ROM_PAGE_SIZE;
        let chr_rom_size = header.chr_rom_size as usize * CHR_ROM_PAGE_SIZE;

        let prg_rom_start = 16 + if should_skip_trainer { 512 } else { 0 };
        let chr_rom_start = prg_rom_start + prg_rom_size;

        Memory {
            ram: [0; 2048],
            prg_rom: raw_data[prg_rom_start..(prg_rom_start + prg_rom_size)].to_vec(),
            chr_rom: raw_data[chr_rom_start..(chr_rom_start + chr_rom_size)].to_vec(),
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
            0x8000..=0xFFFF => self.read_prg_rom(addr),
            _ => {
                // panic!("Error: Unknown Memory Address")
                0
            }
        }
    }

    pub fn write_mem(&mut self, addr: u16, value: u8) {
        match addr {
            RAM_START..=RAM_MIRROR_END => self.ram[(addr) as usize] = value,
            _ => {
                // panic!("Error: Unknown Memory Address")
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
