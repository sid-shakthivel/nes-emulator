#![allow(warnings)]

#[macro_use]
extern crate lazy_static;

use std::cell::RefCell;
use std::fs::File;
use std::io::Read;
use std::mem;
use std::rc::Rc;
use std::time::Duration;

use controller::Controller;
use memory::Memory;
use ppu::frame::Frame;
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

use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();

    assert!(args.len() == 2);

    // Setup ROM
    let rom_filename = &args[1];
    let romname = &rom_filename[0..rom_filename.len() - 4];

    let mut rom_file = File::open(&rom_filename).expect("Error: Cannot find ROM file");
    let rom_size = std::fs::metadata(&rom_filename).expect("Error: Cannot read ROM metadata").len() as usize;

    let mut rom_data = vec![0; rom_size];
    rom_file.read(&mut rom_data).expect("Cannot find enough space to read ROM into buffer");

    let rom = rom::ROMHeader::from_vec(&rom_data);
    let (prg_rom, chr_rom) = rom.verify_and_extract();
    let mirroring_type = rom.get_mirroring_type();

    // Setup SDL

    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();
    let window = video_subsystem.window(&romname, (256.0 * 3.0) as u32, (240.0 * 3.0) as u32).position_centered().build().unwrap();

    let mut canvas = window.into_canvas().present_vsync().build().unwrap();
    let mut event_pump = sdl_context.event_pump().unwrap();
    canvas.set_scale(3.0, 3.0).unwrap();

    let creator = canvas.texture_creator();
    let mut texture = creator.create_texture_target(PixelFormatEnum::RGB24, 256, 240).unwrap();

    // Setup callbackt to display content
    let callback: Box<dyn FnMut(&mut Frame, &mut Controller)> = Box::new(move |frame: &mut Frame, controller: &mut Controller| {
        texture.update(None, &frame.pixels, 256 * 3).unwrap();

        canvas.copy(&texture, None, None).unwrap();
        canvas.present();

        // Handle events here instead of inside the closure
        while let Some(event) = event_pump.poll_event()
        {
            match event
            {
                Event::Quit { .. } | Event::KeyDown { keycode: Some(Keycode::Escape), .. } => std::process::exit(0),
                Event::KeyDown { keycode, .. } =>
                {
                    if let Some(key) = keycode
                    {
                        controller.set_controller_key(key, true);
                    }
                }
                Event::KeyUp { keycode, .. } =>
                {
                    if let Some(key) = keycode
                    {
                        controller.set_controller_key(key, false);
                    }
                }
                _ =>
                {}
            }
        }
    });

    // Setup emulator components
    let controller_a = Rc::new(RefCell::new(controller::Controller::new()));

    let ppu = Rc::new(RefCell::new(ppu::PPU::new(chr_rom, mirroring_type, Rc::clone(&controller_a), callback)));
    let memory = Rc::new(RefCell::new(Memory::new(prg_rom, Rc::clone(&ppu), Rc::clone(&controller_a))));

    let mut cpu = CPU::new(memory, Rc::clone(&ppu));
    cpu.reset();
    cpu.run();
}
