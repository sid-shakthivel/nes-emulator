use bitflags::bitflags;

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
        if self.contains(PpuCtrl::VRAM_ADDR_INC)
        {
            32
        }
        else
        {
            1
        }
    }

    pub fn nametable_addr(&self) -> u16 {
        match self.bits() & PpuCtrl::NAMETABLE.bits()
        {
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

pub struct AddrRegister {
    low_byte: u8,
    high_byte: u8,
    pub is_high: bool,
    mask: u16,
}

impl AddrRegister {
    pub fn new() -> Self {
        AddrRegister {
            low_byte: 0,
            high_byte: 0,
            is_high: true,
            mask: 0x3FFF,
        }
    }

    pub fn update(&mut self, value: u8) {
        if self.is_high
        {
            self.high_byte = value;
        }
        else
        {
            self.low_byte = value;
        }

        self.is_high = !self.is_high;
    }

    pub fn increment(&mut self, value: u8) {
        let lo = self.low_byte;
        self.low_byte = self.low_byte.wrapping_add(value);
        if lo > self.low_byte
        {
            self.high_byte = self.high_byte.wrapping_add(1);
        }
    }

    pub fn set(&mut self, value: u16) {
        self.low_byte = (value & 0xFF) as u8;
        self.high_byte = (value >> 8) as u8;
    }

    pub fn reset_latch(&mut self) {
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
        if !self.is_high
        {
            self.scroll_x = data;
        }
        else
        {
            self.scroll_y = data;
        }
        self.is_high = !self.is_high;
    }

    pub fn reset_latch(&mut self) {
        self.is_high = false;
    }
}
