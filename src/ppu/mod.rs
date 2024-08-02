use std::cell::RefCell;
use std::rc::Rc;

use bitflags::bitflags;
use bitflags::Flags;

use frame::Frame;
use registers::{AddrRegister, PpuCtrl, PpuMask, PpuStatus, ScrollRegister};

use crate::controller::Controller;
use crate::rom::MirroringType;

pub mod frame;
mod palette;
mod registers;

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

bitflags! {
    pub struct SpriteAttributes: u8 {
        const PALETTE =     0b00000011;
        const UNUSED =      0b00011100;
        const PRIORITY =    0b00100000;
        const FLIP_HORIZONTAL = 0b01000000;
        const FLIP_VERTICAL = 0b10000000;
    }
}

pub struct PPU<'call> {
    chr_rom: Vec<u8>,
    palette_table: [u8; 32],
    vram: [u8; 2048],
    oam_data: [u8; 256],

    mirroring: MirroringType,
    addr_reg: AddrRegister,
    ctrl_reg: PpuCtrl,
    mask_reg: PpuMask,
    status_reg: PpuStatus,
    scroll_reg: ScrollRegister,

    data_buffer: u8,
    oam_addr: u8,
    cycles: u32,
    scanline: u16,
    nmi_interrupt: u8,
    callback: Box<dyn FnMut(&mut Frame, &mut Controller) + 'call>,

    controller: Rc<RefCell<Controller>>,
}

impl<'a> PPU<'a> {
    pub fn new<'call, F>(chr_rom: Vec<u8>, mirroring: MirroringType, controller: Rc<RefCell<Controller>>, callback: F) -> PPU<'call>
    where
        F: FnMut(&mut Frame, &mut Controller) + 'call,
    {
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
            controller,
            callback: Box::from(callback),
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

    fn is_sprite_0_hit(&self, cycle: u32) -> bool {
        let y = self.oam_data[0] as u32;
        let x = self.oam_data[3] as u32;
        (y == self.scanline as u32) && x <= cycle && self.mask_reg.show_sprites()
    }

    fn mirror_nametable_addr(&self, addr: u16) -> u16 {
        match self.mirroring
        {
            MirroringType::Horizontal => match addr
            {
                0x2000..=0x23FF => addr - 0x2000,
                0x2400..=0x27FF => addr - 0x2400,
                0x2800..=0x2BFF => addr - 0x2800 + 0x400,
                0x2C00..=0x2FFF => addr - 0x2C00 + 0x400,
                _ => panic!("Error: Invalid nametable address"),
            },
            MirroringType::Vertical => match addr
            {
                0x2000..=0x23FF => addr - 0x2000,
                0x2400..=0x27FF => addr - 0x2400 + 0x400,
                0x2800..=0x2BFF => addr - 0x2800,
                0x2C00..=0x2FFF => addr - 0x2C00 + 0x400,
                _ => panic!("Error: Invalid nametable address"),
            },
            _ => panic!("Error: Invalid mirroring type"),
        }
    }

    pub fn read_reg(&mut self, addr: u16) -> u8 {
        match addr
        {
            0x2000 | 0x2001 | 0x2003 | 0x2005 | 0x2006 | 0x4014 => panic!("Error: Attempt to read from write-only PPU address {:x}", addr),
            0x2002 =>
            {
                // Read Status Register
                let data = self.status_reg.get();
                self.addr_reg.reset_latch();
                self.status_reg.reset_vblank();
                self.scroll_reg.reset_latch();
                data
            }
            0x2004 =>
            {
                // Read OAM Data
                self.oam_addr = self.oam_addr.wrapping_add(1);
                self.oam_data[self.oam_addr as usize]
            }
            0x2007 => self.read_data(),
            _ => panic!("Error: Unknown Memory Address {:#X}", addr),
        }
    }

    pub fn write_reg(&mut self, addr: u16, value: u8) {
        match addr
        {
            0x2000 =>
            {
                let before_nmi_status = self.ctrl_reg.generate_nmi();
                self.ctrl_reg.update(value);

                if !before_nmi_status && self.ctrl_reg.generate_nmi() && self.status_reg.is_vblank()
                {
                    self.nmi_interrupt = 1;
                }
            }
            0x2001 => self.mask_reg.update(value),
            0x2003 => self.oam_addr = value,
            0x2004 =>
            {
                self.oam_data[self.oam_addr as usize] = value;
                self.oam_addr = self.oam_addr.wrapping_add(1);
            }
            0x2005 => self.scroll_reg.write(value),
            0x2006 => self.addr_reg.update(value),
            0x2007 => self.write_data(value),
            _ => panic!("Error: Unknown Memory Address {:#X}", addr),
        }
    }

    fn read_data(&mut self) -> u8 {
        let addr = self.addr_reg.get();
        self.increment_vram_addr();

        match addr
        {
            0x00..=0x1fff =>
            {
                let result = self.data_buffer;
                self.data_buffer = self.chr_rom[addr as usize];
                result
            }
            0x2000..=0x3eff =>
            {
                let result = self.data_buffer;
                self.data_buffer = self.vram[self.mirror_nametable_addr(addr) as usize];
                result
            }
            0x3f00..=0x3fff =>
            {
                let addr = addr - 0x3f00;
                self.palette_table[addr as usize]
            }
            _ => panic!("Error: Unknown Memory Address"),
        }
    }

    fn write_data(&mut self, value: u8) {
        let addr = self.addr_reg.get();

        match addr
        {
            0x2000..=0x3eff =>
            {
                self.vram[self.mirror_nametable_addr(addr) as usize] = value;
            }
            0x3f10 | 0x3f14 | 0x3f18 | 0x3f1c =>
            {
                self.palette_table[(addr - 0x10 - 0x3f00) as usize] = value;
            }
            0x3f00..=0x3fff =>
            {
                self.palette_table[addr as usize - 0x3f00] = value;
            }
            _ => panic!("Error: Unknown Memory Address {:?}", addr),
        }

        self.increment_vram_addr();
    }

    pub fn write_oam_dma(&mut self, data: &[u8; 256]) {
        for i in 0..256
        {
            self.oam_data[self.oam_addr as usize] = data[i];
            self.oam_addr = self.oam_addr.wrapping_add(1);
        }
    }

    pub fn update_cycles(&mut self, cycles: u32) {
        self.cycles += cycles * 3;

        let old_nmi = self.nmi_interrupt;

        if self.cycles >= 341
        {
            if self.is_sprite_0_hit(self.cycles)
            {
                self.status_reg.set_sprite_zero_hit(true);
            }

            self.cycles = self.cycles - 341;
            self.scanline += 1;

            if self.scanline == 241
            {
                self.status_reg.set_vblank(true);
                self.status_reg.set_sprite_zero_hit(false);

                if self.ctrl_reg.generate_nmi()
                {
                    self.nmi_interrupt = 1;
                }
            }

            if self.scanline >= 262
            {
                self.scanline = 0;
                self.nmi_interrupt = 0;

                self.status_reg.set_sprite_zero_hit(false);
                self.status_reg.reset_vblank();
            }
        }

        let current_nmi = self.nmi_interrupt;

        if old_nmi == 0 && current_nmi == 1
        {
            let mut frame = self.render();
            (self.callback)(&mut frame, &mut *self.controller.borrow_mut());
        }
    }

    fn bg_palette(&self, tile_col: usize, tile_row: usize, buffer: &Vec<u8>) -> [u8; 4] {
        let attr_table_index = tile_row / 4 * 8 + tile_col / 4;
        let attr_byte = buffer[0x03c0 + attr_table_index];

        let quadrant_row = (tile_row % 4) / 2;
        let quadrant_col = (tile_col % 4) / 2;

        let shift_amount = (quadrant_row * 2 + quadrant_col) * 2;

        let palette_index = (attr_byte >> shift_amount) & 0b11;

        let palette_start: usize = 1 + (palette_index as usize) * 4;

        [self.palette_table[0], self.palette_table[palette_start], self.palette_table[palette_start + 1], self.palette_table[palette_start + 2]]
    }

    fn sprite_palette(&self, pallete_index: u8) -> [u8; 4] {
        let start = 0x11 + (pallete_index * 4) as usize;
        [self.palette_table[0], self.palette_table[start], self.palette_table[start + 1], self.palette_table[start + 2]]
    }

    pub fn render(&self) -> Frame {
        let mut bank = self.ctrl_reg.bg_pt() as usize;
        let nametable_addr = self.ctrl_reg.nametable_addr();

        let mut scroll_x = self.scroll_reg.scroll_x as isize;
        let scroll_y = self.scroll_reg.scroll_y as isize;

        let mut new_frame = Frame::new(&self.chr_rom);

        let mut nametable_a = self.vram[0..0x400].to_vec();
        let mut nametable_b = self.vram[0x400..0x800].to_vec();

        if nametable_addr == 0x2400
        {
            nametable_a = self.vram[0x400..0x800].to_vec();
            nametable_b = self.vram[0..0x400].to_vec();
        }

        // Handle rendering background tiles
        for i in 0..0x03c0
        {
            let mut tile_number = nametable_a[i] as usize;

            let x = i % 32;
            let y = i / 32;

            let palette = self.bg_palette(x, y, &nametable_a);

            new_frame.copy_bg_tile(bank, tile_number, (x * 8, y * 8), &palette, (scroll_x * -1, scroll_y), (scroll_x as usize, 256));
        }

        if scroll_x != 0
        {
            for i in 0..0x03c0
            {
                let mut tile_number = nametable_b[i] as usize;

                let x = i % 32;
                let y = i / 32;

                let palette = self.bg_palette(x, y, &nametable_b);

                new_frame.copy_bg_tile(bank, tile_number, (x * 8, y * 8), &palette, (256 - scroll_x, scroll_y), (0, scroll_x as usize));
            }
        }

        // Handle rendering sprites
        bank = self.ctrl_reg.sprite_pt() as usize;

        for i in 0..64
        {
            let y_pos: usize = self.oam_data[i * 4 + 0] as usize + 1;
            let tile_number = self.oam_data[i * 4 + 1] as usize;
            let attributes = SpriteAttributes::from_bits_retain(self.oam_data[i * 4 + 2]);
            let x_pos = self.oam_data[i * 4 + 3] as usize;

            let palette_index = (attributes.bits()) & 0b11;

            let palette = self.sprite_palette(palette_index);
            let flip_horizonal = attributes.contains(SpriteAttributes::FLIP_HORIZONTAL);
            let flip_vertical = attributes.contains(SpriteAttributes::FLIP_VERTICAL);

            new_frame.copy_sprite_tile(bank, tile_number, (x_pos, y_pos), (flip_horizonal, flip_vertical), &palette);
        }

        new_frame
    }
}
