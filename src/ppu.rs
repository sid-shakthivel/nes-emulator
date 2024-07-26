use bitflags::BitFlags;

use bitflags::bitflags;
use bitflags::Flags;

use crate::rom::MirroringType;

/*
    PPU Memory Map

    0x0000-0x0FFF - Pattern Table 0
    0x1000-0x1FFF - Pattern Table 1
    0x2000-0x23FF - Name Table 0
    0x2400-0x27FF - Name Table 1
    0x2800-0x2BFF - Name Table 2 (mirrored)
    0x2C00-0x2FFF - Name Table 3 (mirrored)
    0x3000-0x3EFF - Mirrors of $2000 - $2FFF
    0x3F00-0x3FFF - Palette RAM
    0x3F20-0x3FFF - Mirrors of $3F00 - $3F1F
*/

// PPUCTRL
bitflags! {
    pub struct PpuCtrl: u8 {
        const NAMETABLE = 0b00000011;
        const VRAM_ADDR_INC = 0b00000100;
        const SPRITE_PATTERN_TABLE = 0b00001000;
        const BACKGROUND_PATTERN_TABLE = 0b00010000;
        const SPRITE_SIZE = 0b00100000;
        const SLAVE_MASTER_SELECT = 0b01000000;
        const NMI_ENABLE = 0b10000000;
    }
}

impl PpuCtrl {
    pub fn new() -> Self {
        PpuCtrl::empty()
    }

    pub fn update(&mut self, value: u8) {
        *self = PpuCtrl::from_bits_truncate(value);
    }

    pub fn get_inc_value(&self) -> u8 {
        if self.contains(PpuCtrl::VRAM_ADDR_INC) {
            32
        } else {
            1
        }
    }

    pub fn nametable_addr(&self) -> u16 {
        match self.bits() & PpuCtrl::NAMETABLE.bits() {
            0 => 0x2000,
            1 => 0x2400,
            2 => 0x2800,
            3 => 0x2C00,
            _ => panic!("Error: Invalid nametable address"),
        }
    }

    pub fn generate_nmi(&self) -> bool {
        self.contains(PpuCtrl::NMI_ENABLE)
    }
}

// PPUMASK
bitflags! {
    pub struct PpuMask: u8 {
        const GREYSCALE = 0b00000001;
        const SHOW_BACKGROUND_LEFT = 0b00000010;
        const SHOW_SPRITES_LEFT = 0b00000100;
        const SHOW_BACKGROUND = 0b00001000;
        const SHOW_SPRITES = 0b00010000;
        const EMPHASISE_RED = 0b00100000;
        const EMPHASISE_GREEN = 0b01000000;
        const EMPHASISE_BLUE = 0b10000000;
    }
}

impl PpuMask {
    pub fn new() -> Self {
        PpuMask::empty()
    }

    pub fn update(&mut self, value: u8) {
        *self = PpuMask::from_bits_truncate(value);
    }
}

// PPUSTATUS
bitflags! {
    pub struct PpuStatus: u8 {
        const UNUSED = 0b00011111;
        const SPRITE_OVERFLOW = 0b00100000;
        const SPRITE0_HIT = 0b01000000;
        const VBLANK = 0b10000000;
    }
}

impl PpuStatus {
    pub fn new() -> Self {
        PpuStatus::empty()
    }

    pub fn reset_vblank(&mut self) {
        self.remove(PpuStatus::VBLANK);
    }

    pub fn get(&self) -> u8 {
        self.bits()
    }
}

struct AddrRegister {
    low_byte: u8,
    high_byte: u8,
    is_high: bool,
    mask: u16,
}

impl AddrRegister {
    fn new() -> Self {
        AddrRegister {
            low_byte: 0,
            high_byte: 0,
            is_high: true,
            mask: 0x3FFF,
        }
    }

    pub fn update(&mut self, value: u8) {
        match self.is_high {
            true => {
                self.high_byte = value;
                self.is_high = false;
            }
            false => {
                self.low_byte = value;
                self.is_high = true;
            }
        }

        self.set(self.mask());
    }

    pub fn increment(&mut self, value: u8) {
        let value = self.get();
        self.set(value.wrapping_add(1));
        self.set(self.mask());
    }

    pub fn set(&mut self, value: u16) {
        self.low_byte = (value & 0xFF) as u8;
        self.high_byte = ((value >> 8) & 0xFF) as u8;
    }

    // Method used to mask address to maximum of 0x3FFF
    pub fn mask(&self) -> u16 {
        self.mask & self.get()
    }

    pub fn reset_is_high(&mut self) {
        self.is_high = true;
    }

    pub fn get(&self) -> u16 {
        ((self.high_byte as u16) << 8) | (self.low_byte as u16)
    }
}

pub struct PPU {
    pub chr_rom: Vec<u8>,
    pub palette_table: [u8; 32],
    pub vram: [u8; 2048],
    pub oam_data: [u8; 256],
    pub mirroring: MirroringType,
    addr_reg: AddrRegister,
    ctrl_reg: PpuCtrl,
    mask_reg: PpuMask,
    status_reg: PpuStatus,
    data_buffer: u8,
    pub oam_addr: u8,
}

impl PPU {
    pub fn new(chr_rom: Vec<u8>, mirroring: MirroringType) -> Self {
        PPU {
            chr_rom,
            palette_table: [0; 32],
            vram: [0; 2048],
            oam_data: [0; 256],
            addr_reg: AddrRegister::new(),
            ctrl_reg: PpuCtrl::new(),
            mask_reg: PpuMask::new(),
            status_reg: PpuStatus::new(),
            data_buffer: 0,
            mirroring,
            oam_addr: 0,
        }
    }

    fn increment_vram_addr(&mut self) {
        self.addr_reg.increment(self.ctrl_reg.get_inc_value());
    }

    pub fn write_addr_reg(&mut self, value: u8) {
        self.addr_reg.update(value);
    }

    pub fn write_ctrl_reg(&mut self, value: u8) {
        self.ctrl_reg.update(value);
    }

    pub fn write_mask_reg(&mut self, value: u8) {
        self.mask_reg.update(value);
    }

    pub fn write_oam_addr(&mut self, value: u8) {
        self.oam_addr = value;
    }

    pub fn read_status_reg(&mut self) -> u8 {
        let data = self.status_reg.get();
        self.addr_reg.reset_is_high();
        self.status_reg.reset_vblank();
        data
    }

    pub fn read_oam_data(&self) -> u8 {
        self.oam_data[self.oam_addr as usize]
    }

    pub fn write_oam_data(&mut self, value: u8) {
        self.oam_data[self.oam_addr as usize] = value;
        self.oam_addr = self.oam_addr.wrapping_add(1);
    }

    fn mirror_nametable_addr(&self, addr: u16) -> u16 {
        match self.mirroring {
            MirroringType::Horizontal => match addr {
                0x2000..=0x23FF => addr - 0x2000,
                0x2400..=0x27FF => addr - 0x2400,
                0x2800..=0x2BFF => addr - 0x2800 + 0x1000,
                0x2C00..=0x2FFF => addr - 0x2C00 + 0x1000,
                _ => panic!("Error: Invalid nametable address"),
            },
            MirroringType::Vertical => match addr {
                0x2000..=0x23FF => addr - 0x2000,
                0x2400..=0x27FF => addr - 0x2400 + 0x1000,
                0x2800..=0x2BFF => addr - 0x2800,
                0x2C00..=0x2FFF => addr - 0x2C00 + 0x1000,
                _ => panic!("Error: Invalid nametable address"),
            },
            _ => panic!("Error: Invalid mirroring type"),
        }
    }

    pub fn read(&mut self) -> u8 {
        let addr = self.addr_reg.get();
        self.increment_vram_addr();

        match addr {
            0x00..=0x1fff => {
                let result = self.data_buffer;
                self.data_buffer = self.chr_rom[addr as usize];
                result
            }
            0x2000..=0x3eff => {
                let result = self.data_buffer;
                self.data_buffer = self.vram[self.mirror_nametable_addr(addr) as usize];
                result
            }
            0x3f00..=0x3fff => {
                let addr = addr - 0x3f00;
                self.palette_table[addr as usize]
            }
            _ => panic!("Error: Unknown Memory Address"),
        }
    }

    pub fn write_oam_dma(&mut self, data: &[u8; 256]) {
        for i in 0..256 {
            self.oam_data[self.oam_addr as usize] = data[i];
            self.oam_addr = self.oam_addr.wrapping_add(1);
        }
    }

    pub fn write(&mut self) {}
}
