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
    chr_rom: Vec<u8>,
}

impl Frame {
    const WIDTH: usize = 256;
    const HIGHT: usize = 240;

    pub fn new(chr_rom: &Vec<u8>) -> Self {
        Frame {
            pixels: vec![0; (Frame::WIDTH) * (Frame::HIGHT) * 3],
            chr_rom: chr_rom.to_vec(),
        }
    }

    fn set_pixel(&mut self, x: usize, y: usize, rgb: (u8, u8, u8)) {
        let base = y * 3 * Frame::WIDTH + x * 3;

        // assert!(base + 2 < self.pixels.len());

        if base + 2 > self.pixels.len() {
            return;
        }

        self.pixels[base] = rgb.0;
        self.pixels[base + 1] = rgb.1;
        self.pixels[base + 2] = rgb.2;
    }

    pub fn copy_bg_tile(
        &mut self,
        bank: usize,
        tile_number: usize,
        tile_coords: (usize, usize),
        palette: &[u8; 4],
        offset: (isize, isize),
        x_limits: (usize, usize),
    ) {
        assert!(bank <= 1);
        let bank = (bank * 0x1000) as usize;

        let tile_a_offset = bank + tile_number * 16;
        let tile_b_offset = tile_a_offset + 8;

        let (scroll_x, scroll_y) = offset;

        let (tile_x, tile_y) = tile_coords;

        for i in (0..8) {
            let mut upper_byte = self.chr_rom[tile_a_offset + i];
            let mut lower_byte = self.chr_rom[tile_a_offset + i + 8];

            for j in (0..8).rev() {
                let upper_bit = (upper_byte >> j) & 1;
                let lower_bit = (lower_byte >> j) & 1;

                let colour_index = ((1 & lower_bit) << 1) | (upper_bit & 1);

                let colour = SDL_COLOUR_PALLETE[palette[colour_index as usize] as usize];

                let x_offset = 7 - j;

                let x_value = (x_offset + tile_x);

                if x_value >= x_limits.0 && x_value < x_limits.1 {
                    self.set_pixel((x_value as isize + scroll_x) as usize, i + tile_y, colour);
                }
            }
        }
    }

    pub fn copy_sprite_tile(
        &mut self,
        bank: usize,
        tile_number: usize,
        tile_coords: (usize, usize),
        tile_mods: (bool, bool),
        palette: &[u8; 4],
    ) {
        assert!(bank <= 1);
        let bank = (bank * 0x1000) as usize;

        let tile_a_offset = bank + tile_number * 16;
        let tile_b_offset = tile_a_offset + 8;

        let (tile_x, tile_y) = tile_coords;
        let (flip_horizontal, flip_vertical) = tile_mods;

        for i in (0..8) {
            let mut upper_byte = self.chr_rom[tile_a_offset + i];
            let mut lower_byte = self.chr_rom[tile_a_offset + i + 8];

            for j in (0..8) {
                let upper_bit = (upper_byte >> j) & 1;
                let lower_bit = (lower_byte >> j) & 1;

                let colour_index = ((1 & lower_bit) << 1) | (upper_bit & 1);

                let colour = SDL_COLOUR_PALLETE[palette[colour_index as usize] as usize];

                if colour_index == 0 {
                    continue;
                }

                let x_offset = 7 - j;

                match (flip_horizontal, flip_vertical) {
                    (false, false) => self.set_pixel(tile_x + x_offset, tile_y + i, colour),
                    (true, false) => self.set_pixel(tile_x + 7 - x_offset, tile_y + i, colour),
                    (false, true) => self.set_pixel(tile_x + x_offset, tile_y + 7 - i, colour),
                    (true, true) => self.set_pixel(tile_x + 7 - x_offset, tile_y + 7 - i, colour),
                }
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

    pub fn sprite_pt(&self) -> u16 {
        self.contains(PpuCtrl::SPRITE_PATTERN_TABLE) as u16
    }

    pub fn sprite_size(&self) -> u16 {
        self.contains(PpuCtrl::SPRITE_PATTERN_TABLE) as u16
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

    pub fn show_sprites(&self) -> bool {
        self.contains(PpuMask::SHOW_SPRITES)
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
    pub is_high: bool,
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

        // self.set(self.mask());
    }

    pub fn increment(&mut self, value: u8) {
        let lo = self.low_byte;
        self.low_byte = self.low_byte.wrapping_add(value);
        if lo > self.low_byte {
            self.high_byte = self.high_byte.wrapping_add(1);
        }
    }

    pub fn set(&mut self, value: u16) {
        self.low_byte = (value & 0xFF) as u8;
        self.high_byte = (value >> 8) as u8;
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

bitflags! {
    pub struct SpriteAttributes: u8 {
        const PALETTE =     0b00000011;
        const UNUSED =      0b00011100;
        const PRIORITY =    0b00100000;
        const FLIP_HORIZONTAL = 0b01000000;
        const FLIP_VERTICAL = 0b10000000;
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
    scanline: u16,
    pub nmi_interrupt: u8,
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
            scanline: 0,
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
        let old = self.addr_reg.get().wrapping_sub(0x2000);
        self.addr_reg.update(value);
    }

    pub fn write_ctrl_reg(&mut self, value: u8) {
        let before_nmi_status = self.ctrl_reg.generate_nmi();
        self.ctrl_reg.update(value);

        if !before_nmi_status && self.ctrl_reg.generate_nmi() && self.status_reg.is_vblank() {
            self.nmi_interrupt = 1;
        }
    }

    pub fn write_mask_reg(&mut self, value: u8) {
        self.mask_reg.update(value);
    }

    pub fn write_oam_addr(&mut self, value: u8) {
        self.oam_addr = value;
    }

    pub fn write_scroll_addr(&mut self, value: u8) {
        self.scroll_reg.write(value);
    }

    pub fn read_status_reg(&mut self) -> u8 {
        let data = self.status_reg.get();
        self.addr_reg.reset_is_high();
        self.status_reg.reset_vblank();
        self.scroll_reg.reset_latch();
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

    fn is_sprite_0_hit(&self, cycle: u32) -> bool {
        let y = self.oam_data[0] as u32;
        let x = self.oam_data[3] as u32;
        (y == self.scanline as u32) && x <= cycle && self.mask_reg.show_sprites()
    }

    fn mirror_nametable_addr(&self, addr: u16) -> u16 {
        match self.mirroring {
            MirroringType::Horizontal => match addr {
                0x2000..=0x23FF => addr - 0x2000,
                0x2400..=0x27FF => addr - 0x2400,
                0x2800..=0x2BFF => addr - 0x2800 + 0x400,
                0x2C00..=0x2FFF => addr - 0x2C00 + 0x400,
                _ => panic!("Error: Invalid nametable address"),
            },
            MirroringType::Vertical => match addr {
                0x2000..=0x23FF => addr - 0x2000,
                0x2400..=0x27FF => addr - 0x2400 + 0x400,
                0x2800..=0x2BFF => addr - 0x2800,
                0x2C00..=0x2FFF => addr - 0x2C00 + 0x400,
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

    pub fn write(&mut self, value: u8) {
        // panic!("random write");
        let addr = self.addr_reg.get();

        match addr {
            0x2000..=0x3eff => {
                // println!(
                //     "offset {:#X} value {:#X}",
                //     self.mirror_nametable_addr(addr),
                //     value,
                // );
                self.vram[self.mirror_nametable_addr(addr) as usize] = value;
            }
            0x3f10 | 0x3f14 | 0x3f18 | 0x3f1c => {
                self.palette_table[(addr - 0x10 - 0x3f00) as usize] = value;
            }
            0x3f00..=0x3fff => {
                let offset = addr - 0x3f00;
                // println!("palette stuff {} {}", offset, value);
                self.palette_table[offset as usize] = value;
            }
            _ => panic!("Error: Unknown Memory Address"),
        }

        self.increment_vram_addr();
    }

    pub fn update_cycles(&mut self, cycles: u32) -> bool {
        self.cycles += cycles * 3;

        if self.cycles >= 341 {
            if self.is_sprite_0_hit(self.cycles) {
                self.status_reg.set_sprite_zero_hit(true);
            }

            self.cycles = self.cycles - 341;
            self.scanline += 1;

            if self.scanline == 241 {
                self.status_reg.set_vblank(true);
                self.status_reg.set_sprite_zero_hit(false);

                if self.ctrl_reg.generate_nmi() {
                    self.nmi_interrupt = 1;
                }
            }

            if self.scanline >= 262 {
                self.scanline = 0;
                self.nmi_interrupt = 0;

                self.status_reg.set_sprite_zero_hit(false);
                self.status_reg.reset_vblank();

                return true;
            }
        }

        return false;
    }

    fn bg_palette(&self, tile_col: usize, tile_row: usize, buffer: &Vec<u8>) -> [u8; 4] {
        let attr_table_index = tile_row / 4 * 8 + tile_col / 4;
        let attr_byte = buffer[0x03c0 + attr_table_index];

        let quadrant_row = (tile_row % 4) / 2;
        let quadrant_col = (tile_col % 4) / 2;

        let shift_amount = (quadrant_row * 2 + quadrant_col) * 2;

        let palette_index = (attr_byte >> shift_amount) & 0b11;

        let palette_start: usize = 1 + (palette_index as usize) * 4;

        [
            self.palette_table[0],
            self.palette_table[palette_start],
            self.palette_table[palette_start + 1],
            self.palette_table[palette_start + 2],
        ]
    }

    fn sprite_palette(&self, pallete_index: u8) -> [u8; 4] {
        let start = 0x11 + (pallete_index * 4) as usize;
        [
            self.palette_table[0],
            self.palette_table[start],
            self.palette_table[start + 1],
            self.palette_table[start + 2],
        ]
    }

    pub fn render(&self) -> Frame {
        let mut bank = self.ctrl_reg.bg_pt() as usize;
        let nametable_addr = self.ctrl_reg.nametable_addr();

        let mut scroll_x = self.scroll_reg.scroll_x as isize;
        let scroll_y = self.scroll_reg.scroll_y as isize;

        // assert!(nametable_addr == 0x2000);

        let mut new_frame = Frame::new(&self.chr_rom);

        let mut nametable_a = self.vram[0..0x400].to_vec();
        let mut nametable_b = self.vram[0x400..0x800].to_vec();

        if nametable_addr == 0x2400 {
            // println!("here?");
            nametable_a = self.vram[0x400..0x800].to_vec();
            nametable_b = self.vram[0..0x400].to_vec();
        }

        // Handle rendering background tiles
        for i in 0..0x03c0 {
            let mut tile_number = nametable_a[i] as usize;

            let x = i % 32;
            let y = i / 32;

            let palette = self.bg_palette(x, y, &nametable_a);

            new_frame.copy_bg_tile(
                bank,
                tile_number,
                (x * 8, y * 8),
                &palette,
                (scroll_x * -1, scroll_y),
                (scroll_x as usize, 256),
            );
        }

        if scroll_x != 0 {
            for i in 0..0x03c0 {
                let mut tile_number = nametable_b[i] as usize;

                let x = i % 32;
                let y = i / 32;

                let palette = self.bg_palette(x, y, &nametable_b);

                new_frame.copy_bg_tile(
                    bank,
                    tile_number,
                    (x * 8, y * 8),
                    &palette,
                    (256 - scroll_x, scroll_y),
                    (0, scroll_x as usize),
                );
            }
        }

        // Handle rendering sprites

        // assert!(self.ctrl_reg.sprite_size() == 0);

        bank = self.ctrl_reg.sprite_pt() as usize;

        for i in 0..64 {
            let y_pos: usize = self.oam_data[i * 4 + 0] as usize + 1;
            let tile_number = self.oam_data[i * 4 + 1] as usize;
            let attributes = SpriteAttributes::from_bits_retain(self.oam_data[i * 4 + 2]);
            let x_pos = self.oam_data[i * 4 + 3] as usize;

            let palette_index = (attributes.bits()) & 0b11;

            let palette = self.sprite_palette(palette_index);
            let flip_horizonal = attributes.contains(SpriteAttributes::FLIP_HORIZONTAL);
            let flip_vertical = attributes.contains(SpriteAttributes::FLIP_VERTICAL);

            new_frame.copy_sprite_tile(
                bank,
                tile_number,
                (x_pos, y_pos),
                (flip_horizonal, flip_vertical),
                &palette,
            );
        }

        new_frame
    }
}
