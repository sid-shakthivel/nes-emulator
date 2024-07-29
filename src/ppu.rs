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

pub static SDL_COLOUR_PALLETE: [(u8, u8, u8); 64] = [
    (0x80, 0x80, 0x80),
    (0x00, 0x3D, 0xA6),
    (0x00, 0x12, 0xB0),
    (0x44, 0x00, 0x96),
    (0xA1, 0x00, 0x5E),
    (0xC7, 0x00, 0x28),
    (0xBA, 0x06, 0x00),
    (0x8C, 0x17, 0x00),
    (0x5C, 0x2F, 0x00),
    (0x10, 0x45, 0x00),
    (0x05, 0x4A, 0x00),
    (0x00, 0x47, 0x2E),
    (0x00, 0x41, 0x66),
    (0x00, 0x00, 0x00),
    (0x05, 0x05, 0x05),
    (0x05, 0x05, 0x05),
    (0xC7, 0xC7, 0xC7),
    (0x00, 0x77, 0xFF),
    (0x21, 0x55, 0xFF),
    (0x82, 0x37, 0xFA),
    (0xEB, 0x2F, 0xB5),
    (0xFF, 0x29, 0x50),
    (0xFF, 0x22, 0x00),
    (0xD6, 0x32, 0x00),
    (0xC4, 0x62, 0x00),
    (0x35, 0x80, 0x00),
    (0x05, 0x8F, 0x00),
    (0x00, 0x8A, 0x55),
    (0x00, 0x99, 0xCC),
    (0x21, 0x21, 0x21),
    (0x09, 0x09, 0x09),
    (0x09, 0x09, 0x09),
    (0xFF, 0xFF, 0xFF),
    (0x0F, 0xD7, 0xFF),
    (0x69, 0xA2, 0xFF),
    (0xD4, 0x80, 0xFF),
    (0xFF, 0x45, 0xF3),
    (0xFF, 0x61, 0x8B),
    (0xFF, 0x88, 0x33),
    (0xFF, 0x9C, 0x12),
    (0xFA, 0xBC, 0x20),
    (0x9F, 0xE3, 0x0E),
    (0x2B, 0xF0, 0x35),
    (0x0C, 0xF0, 0xA4),
    (0x05, 0xFB, 0xFF),
    (0x5E, 0x5E, 0x5E),
    (0x0D, 0x0D, 0x0D),
    (0x0D, 0x0D, 0x0D),
    (0xFF, 0xFF, 0xFF),
    (0xA6, 0xFC, 0xFF),
    (0xB3, 0xEC, 0xFF),
    (0xDA, 0xAB, 0xEB),
    (0xFF, 0xA8, 0xF9),
    (0xFF, 0xAB, 0xB3),
    (0xFF, 0xD2, 0xB0),
    (0xFF, 0xEF, 0xA6),
    (0xFF, 0xF7, 0x9C),
    (0xD7, 0xE8, 0x95),
    (0xA6, 0xED, 0xAF),
    (0xA2, 0xF2, 0xDA),
    (0x99, 0xFF, 0xFC),
    (0xDD, 0xDD, 0xDD),
    (0x11, 0x11, 0x11),
    (0x11, 0x11, 0x11),
];

pub struct Frame {
    pub pixels: Vec<u8>,
}

impl Frame {
    const WIDTH: usize = 256;
    const HIGHT: usize = 240;

    pub fn new() -> Self {
        Frame {
            pixels: vec![0; (Frame::WIDTH) * (Frame::HIGHT) * 3],
        }
    }

    pub fn set_pixel(&mut self, x: usize, y: usize, rgb: (u8, u8, u8)) {
        let base = y * 3 * Frame::WIDTH + x * 3;

        assert!(base + 2 < self.pixels.len());

        self.pixels[base] = rgb.0;
        self.pixels[base + 1] = rgb.1;
        self.pixels[base + 2] = rgb.2;
    }

    pub fn copy_tile(
        &mut self,
        chr_rom: &Vec<u8>,
        bank: usize,
        tile_number: usize,
        x: usize,
        y: usize,
    ) {
        assert!(bank <= 1);
        let bank = (bank * 0x1000) as usize;

        let tile_a_offset = bank + tile_number * 16;
        let tile_b_offset = tile_a_offset + 8;

        for i in (0..8) {
            let mut upper_byte = chr_rom[tile_a_offset + i];
            let mut lower_byte = chr_rom[tile_a_offset + i + 8];

            for j in (0..8).rev() {
                let upper_bit = (upper_byte >> j) & 1;
                let lower_bit = (lower_byte >> j) & 1;

                let color_index = (upper_bit << 1) | lower_bit;

                let colour = match color_index {
                    0 => SDL_COLOUR_PALLETE[0x01],
                    1 => SDL_COLOUR_PALLETE[0x23],
                    2 => SDL_COLOUR_PALLETE[0x27],
                    3 => SDL_COLOUR_PALLETE[0x30],
                    _ => panic!("Error: Unknown color index: {}", color_index),
                };

                let x_offset = 7 - j;

                self.set_pixel(x_offset + x, i + y, colour);
            }
        }
    }
}

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

    pub fn bg_pt(&self) -> u16 {
        self.contains(PpuCtrl::BACKGROUND_PATTERN_TABLE) as u16
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

    pub fn set_vblank(&mut self, value: bool) {
        self.set(PpuStatus::VBLANK, value);
    }

    pub fn reset_vblank(&mut self) {
        self.remove(PpuStatus::VBLANK);
    }

    pub fn is_vblank(&self) -> bool {
        self.contains(PpuStatus::VBLANK)
    }

    pub fn set_sprite_zero_hit(&mut self, value: bool) {
        self.set(PpuStatus::SPRITE0_HIT, value);
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

// PPUSCROLL

pub struct ScrollRegister {
    pub scroll_x: u8,
    pub scroll_y: u8,
    pub is_high: bool,
}

impl ScrollRegister {
    pub fn new() -> Self {
        ScrollRegister {
            scroll_x: 0,
            scroll_y: 0,
            is_high: false,
        }
    }

    pub fn write(&mut self, data: u8) {
        if !self.is_high {
            self.scroll_x = data;
        } else {
            self.scroll_y = data;
        }
        self.is_high = !self.is_high;
    }

    pub fn reset_latch(&mut self) {
        self.is_high = false;
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
    scroll_reg: ScrollRegister,
    data_buffer: u8,
    pub oam_addr: u8,
    cycles: u32,
    current_scanline: u16,
    nmi_interrupt: u8,
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
            scroll_reg: ScrollRegister::new(),
            data_buffer: 0,
            mirroring,
            oam_addr: 0,
            cycles: 0,
            current_scanline: 0,
            nmi_interrupt: 0,
        }
    }

    pub fn get_nmi(&mut self) -> u8 {
        let save = self.nmi_interrupt;
        self.nmi_interrupt = 0;
        save
    }

    fn increment_vram_addr(&mut self) {
        self.addr_reg.increment(self.ctrl_reg.get_inc_value());
    }

    pub fn write_addr_reg(&mut self, value: u8) {
        // panic!("write addr register");
        self.addr_reg.update(value);
    }

    pub fn write_ctrl_reg(&mut self, value: u8) {
        // println!("write ctrl register {:b}", value);
        let before_nmi_status = self.ctrl_reg.generate_nmi();
        self.ctrl_reg.update(value);

        if !before_nmi_status && self.ctrl_reg.generate_nmi() && self.status_reg.is_vblank() {
            self.nmi_interrupt = 1;
        }
    }

    pub fn write_mask_reg(&mut self, value: u8) {
        // println!("write mask register {:b}", value);
        self.mask_reg.update(value);
    }

    pub fn write_oam_addr(&mut self, value: u8) {
        // panic!("write oam addr {}", value);
        self.oam_addr = value;
    }

    pub fn write_scroll_addr(&mut self, value: u8) {
        // println!("write scroll addr {:b}", value);
        self.scroll_reg.write(value);
    }

    pub fn read_status_reg(&mut self) -> u8 {
        let data = self.status_reg.get();
        self.addr_reg.reset_is_high();
        self.status_reg.reset_vblank();

        // println!("read status reg {:b}", data);

        data
    }

    pub fn read_oam_data(&self) -> u8 {
        panic!("read oam data");
        self.oam_data[self.oam_addr as usize]
    }

    pub fn write_oam_data(&mut self, value: u8) {
        panic!("write oam data");
        self.oam_data[self.oam_addr as usize] = value;
        self.oam_addr = self.oam_addr.wrapping_add(1);
    }

    fn mirror_nametable_addr(&self, addr: u16) -> u16 {
        match self.mirroring {
            MirroringType::Horizontal => match addr {
                0x2000..=0x23FF => addr - 0x2000,
                0x2400..=0x27FF => addr - 0x2400,
                0x2800..=0x2BFF => addr - 0x2800 + 0x100,
                0x2C00..=0x2FFF => addr - 0x2C00 + 0x100,
                _ => panic!("Error: Invalid nametable address"),
            },
            MirroringType::Vertical => match addr {
                0x2000..=0x23FF => addr - 0x2000,
                0x2400..=0x27FF => addr - 0x2400 + 0x100,
                0x2800..=0x2BFF => addr - 0x2800,
                0x2C00..=0x2FFF => addr - 0x2C00 + 0x100,
                _ => panic!("Error: Invalid nametable address"),
            },
            _ => panic!("Error: Invalid mirroring type"),
        }
    }

    pub fn read(&mut self) -> u8 {
        // panic!("reading");

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

    pub fn write(&mut self, value: u8) {
        // panic!("random write");
        let addr = self.addr_reg.get();

        match addr {
            0x2000..=0x3eff => {
                // println!(
                //     "addr = {:#X} and {:#X}",
                //     addr,
                //     self.mirror_nametable_addr(addr)
                // );
                self.vram[self.mirror_nametable_addr(addr) as usize] = value;
            }
            0x3f00..=0x3fff => {
                let addr = addr - 0x3f00;
                self.palette_table[addr as usize] = value;
            }
            _ => panic!("Error: Unknown Memory Address"),
        }

        self.increment_vram_addr();
    }

    pub fn update_cycles(&mut self, cycles: u32) -> bool {
        self.cycles += cycles * 3;

        if self.cycles >= 341 {
            self.cycles = self.cycles - 341;
            self.current_scanline += 1;

            if self.current_scanline == 241 {
                self.status_reg.set_vblank(true);
                self.status_reg.set_sprite_zero_hit(false);

                if self.ctrl_reg.generate_nmi() {
                    self.nmi_interrupt = 1;
                }
            }

            if self.current_scanline >= 262 {
                self.current_scanline = 0;
                self.nmi_interrupt = 0;

                self.status_reg.set_sprite_zero_hit(false);
                self.status_reg.reset_vblank();

                return true;
            }
        }

        return false;
    }

    pub fn render(&self) -> Frame {
        let bank = self.ctrl_reg.bg_pt() as usize;
        let nametable_addr = self.ctrl_reg.nametable_addr();

        let mut new_frame = Frame::new();

        let mut is_not_zero = false;

        for i in 0..0x03c0 {
            let tile_number = self.vram[i] as usize;

            if tile_number != 0 {
                // println!("tile {}", tile_number);
                is_not_zero = true;
            }

            let x = (i % 32) * 8;
            let y = (i / 32) * 8;

            new_frame.copy_tile(&self.chr_rom, bank, tile_number, x, y);
        }

        if is_not_zero {
            // panic!("dinito");
        }

        new_frame
    }
}
