use crate::ppu::palette::SDL_COLOUR_PALLETE;

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

        if base + 2 > self.pixels.len()
        {
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

        for i in (0..8)
        {
            let mut upper_byte = self.chr_rom[tile_a_offset + i];
            let mut lower_byte = self.chr_rom[tile_b_offset + i];

            for j in (0..8).rev()
            {
                let upper_bit = (upper_byte >> j) & 1;
                let lower_bit = (lower_byte >> j) & 1;

                let colour_index = ((1 & lower_bit) << 1) | (upper_bit & 1);

                let colour = SDL_COLOUR_PALLETE[palette[colour_index as usize] as usize];

                let x_offset = 7 - j;

                let x_value = (x_offset + tile_x);

                if x_value >= x_limits.0 && x_value < x_limits.1
                {
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

        for i in (0..8)
        {
            let mut upper_byte = self.chr_rom[tile_a_offset + i];
            let mut lower_byte = self.chr_rom[tile_b_offset + i];

            for j in (0..8)
            {
                let upper_bit = (upper_byte >> j) & 1;
                let lower_bit = (lower_byte >> j) & 1;

                let colour_index = ((1 & lower_bit) << 1) | (upper_bit & 1);

                let colour = SDL_COLOUR_PALLETE[palette[colour_index as usize] as usize];

                if colour_index == 0
                {
                    continue;
                }

                let x_offset = 7 - j;

                match (flip_horizontal, flip_vertical)
                {
                    (false, false) => self.set_pixel(tile_x + x_offset, tile_y + i, colour),
                    (true, false) => self.set_pixel(tile_x + 7 - x_offset, tile_y + i, colour),
                    (false, true) => self.set_pixel(tile_x + x_offset, tile_y + 7 - i, colour),
                    (true, true) => self.set_pixel(tile_x + 7 - x_offset, tile_y + 7 - i, colour),
                }
            }
        }
    }
}
