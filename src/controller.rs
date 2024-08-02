use bitflags::bitflags;

use sdl2::keyboard::Keycode;

// Controller Buttons
bitflags! {
    pub struct ControllerButtons: u8 {
        const A = 0b00000001;
        const B = 0b00000010;
        const SELECT = 0b00000100;
        const START = 0b00001000;
        const UP = 0b00010000;
        const DOWN = 0b00100000;
        const LEFT = 0b01000000;
        const RIGHT = 0b10000000;
    }
}

pub struct Controller {
    strobe: bool,
    state: ControllerButtons,
    index: u8,
}

impl Controller {
    pub fn new() -> Self {
        Controller {
            strobe: false,
            state: ControllerButtons::empty(),
            index: 0,
        }
    }

    pub fn write(&mut self, value: u8) {
        self.strobe = (value & 1) != 0;
        if self.strobe
        {
            self.index = 0;
        }
    }

    pub fn read(&mut self) -> u8 {
        let button = match self.index
        {
            0 => ControllerButtons::A,
            1 => ControllerButtons::B,
            2 => ControllerButtons::SELECT,
            3 => ControllerButtons::START,
            4 => ControllerButtons::UP,
            5 => ControllerButtons::DOWN,
            6 => ControllerButtons::LEFT,
            7 => ControllerButtons::RIGHT,
            _ => ControllerButtons::empty(),
        };

        self.index = (self.index + 1) % 8;

        return self.state.contains(button) as u8;
    }

    pub fn set_controller_key(&mut self, keycode: Keycode, value: bool) {
        match keycode
        {
            Keycode::A => self.state.set(ControllerButtons::A, value),
            Keycode::S => self.state.set(ControllerButtons::B, value),
            Keycode::Return => self.state.set(ControllerButtons::START, value),
            Keycode::Space => self.state.set(ControllerButtons::SELECT, value),
            Keycode::Up => self.state.set(ControllerButtons::UP, value),
            Keycode::Down => self.state.set(ControllerButtons::DOWN, value),
            Keycode::Left => self.state.set(ControllerButtons::LEFT, value),
            Keycode::Right => self.state.set(ControllerButtons::RIGHT, value),
            _ =>
            {}
        }
    }
}
