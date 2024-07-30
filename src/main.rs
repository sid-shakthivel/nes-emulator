#![allow(warnings)]

#[macro_use]
extern crate lazy_static;

use std::cell::RefCell;
use std::fs::File;
use std::io::Read;
use std::mem;
use std::rc::Rc;
use std::time::Duration;

use memory::Memory;
use ppu::Frame;
use rand::rngs::StdRng;
use rand::Rng;
use rand::SeedableRng;

use cpu::CPU;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::Color;
use sdl2::pixels::PixelFormatEnum;
use sdl2::EventPump;

mod controller;
mod cpu;
mod memory;
mod opcodes;
mod ppu;
mod rom;

fn main() {
    // Setup ROM
    let rom_filename = "dk.nes";

    let mut rom_file = File::open(&rom_filename).expect("Error: Cannot find ROM file");
    let rom_size = std::fs::metadata(&rom_filename)
        .expect("Error: Cannot read ROM metadata")
        .len() as usize;

    let mut rom_data = vec![0; rom_size];
    rom_file
        .read(&mut rom_data)
        .expect("Cannot find enough space to read ROM into buffer");

    let rom = rom::ROMHeader::from_vec(&rom_data);
    let (prg_rom, chr_rom) = rom.verify_and_extract();
    let mirroring_type = rom.get_mirroring_type();

    // Setup SDL

    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();
    let window = video_subsystem
        .window("Pacman", (256.0 * 3.0) as u32, (240.0 * 3.0) as u32)
        .position_centered()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas().present_vsync().build().unwrap();
    let mut event_pump = sdl_context.event_pump().unwrap();
    canvas.set_scale(3.0, 3.0).unwrap();

    let creator = canvas.texture_creator();
    let mut texture = creator
        .create_texture_target(PixelFormatEnum::RGB24, 256, 240)
        .unwrap();

    // Setup emulator components
    let ppu = Rc::new(RefCell::new(ppu::PPU::new(chr_rom, mirroring_type)));

    let controller_a = Rc::new(RefCell::new(controller::Controller::new()));

    let mut memory = Memory::new(prg_rom, ppu, controller_a, |ppu, controller| {
        let frame = ppu.render();

        texture.update(None, &frame.pixels, 256 * 3).unwrap();

        canvas.copy(&texture, None, None).unwrap();
        canvas.present();

        // panic!("dono");

        let test = Keycode::A;

        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. }
                | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => std::process::exit(0),

                Event::KeyDown { keycode, .. } => {
                    if let Some(key) = keycode {
                        controller.set_controller_key(key, true);
                    }
                }
                Event::KeyUp { keycode, .. } => {
                    if let Some(key) = keycode {
                        controller.set_controller_key(key, false);
                    }
                }
                _ => { /* do nothing */ }
            }
        }
    });

    let memory_ref = Rc::new(RefCell::new(memory));

    let mut cpu = CPU::new(memory_ref);
    cpu.reset();
    cpu.run();
}
