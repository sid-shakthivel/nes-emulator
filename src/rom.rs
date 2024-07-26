use bitflags::bitflags;

const PRG_ROM_PAGE_SIZE: usize = 16384;
const CHR_ROM_PAGE_SIZE: usize = 8192;

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

pub struct ROMHeader {
    string: [u8; 4],
    prg_rom_size: u8, // Number of banks (16KB)
    chr_rom_size: u8, // Number of banks (8KB)
    control_byte_1: ControlByte1,
    control_byte_2: ControlByte2,
    prg_ram_size: u8,
    reserved: [u8; 5],
    raw_data: Vec<u8>,
}

impl ROMHeader {
    pub fn from_vec(vec: &Vec<u8>) -> ROMHeader {
        ROMHeader {
            string: [vec[0], vec[1], vec[2], vec[3]],
            prg_rom_size: vec[4],
            chr_rom_size: vec[5],
            control_byte_1: ControlByte1::from_bits_retain(vec[6]),
            control_byte_2: ControlByte2::from_bits_retain(vec[7]),
            prg_ram_size: vec[8],
            reserved: [vec[9], vec[10], vec[11], vec[12], vec[13]],
            raw_data: vec.to_vec(),
        }
    }

    pub fn verify_and_extract(&self) -> (Vec<u8>, Vec<u8>) {
        assert!(self.string == [0x4e, 0x45, 0x53, 0x1a]);

        let mapper = self.control_byte_1.bits() & ControlByte1::LOWER_ROM_MAPPER.bits()
            | (self.control_byte_2.bits() & ControlByte2::UPPER_ROM_MAPPER.bits());

        // let test = (raw_data[7] & 0b1111_0000) | (raw_data[6] >> 4);

        // println!("test: {} mapper: {}", test, mapper);

        let should_skip_trainer = self.control_byte_1.contains(ControlByte1::TRAINER);

        // println!(
        //     "{:#X} {:#X}",
        //     header.control_byte_1.bits(),
        //     header.control_byte_2.bits(),
        // );

        assert!(!self.control_byte_2.contains(ControlByte2::FORMAT_1_TYPE));
        assert!(!self.control_byte_2.contains(ControlByte2::FORMAT_2_TYPE));

        // println!("Mirroring: {:?}", screen_mirroring);

        let prg_rom_size = self.prg_rom_size as usize * PRG_ROM_PAGE_SIZE;
        let chr_rom_size = self.chr_rom_size as usize * CHR_ROM_PAGE_SIZE;

        let prg_rom_start = 16 + if should_skip_trainer { 512 } else { 0 };
        let chr_rom_start = prg_rom_start + prg_rom_size;

        return (
            self.raw_data[prg_rom_start..(prg_rom_start + prg_rom_size)].to_vec(),
            self.raw_data[chr_rom_start..(chr_rom_start + chr_rom_size)].to_vec(),
        );
    }

    pub fn get_mirroring_type(&self) -> MirroringType {
        let four_screen = self.control_byte_1.contains(ControlByte1::FOUR_SCREEN_VRAM);
        let vertical_mirroring = self
            .control_byte_1
            .contains(ControlByte1::VERTICAL_MIRRORING);

        match (four_screen, vertical_mirroring) {
            (true, _) => MirroringType::FourScreen,
            (false, true) => MirroringType::Vertical,
            (false, false) => MirroringType::Horizontal,
        }
    }
}
