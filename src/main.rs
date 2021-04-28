extern crate sdl2;

use sdl2::pixels::{Color, PixelFormat, PixelFormatEnum};
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use std::time::{Duration, Instant};

use daisyretro::emulator::rom::ines;
use std::error::Error;
use daisyretro::emulator::famicom::cpu::CPU;
use std::any::Any;
use rand::Rng;
use daisyretro::emulator::famicom::bus::{Bus, MAPPING_MODE_WRITE, MAPPING_MODE_READ};
use daisyretro::emulator::famicom::famicom::Famicom;
use std::env;
use std::num::Wrapping;
use daisyretro::emulator::famicom::joypad::Joypad;
use std::collections::HashMap;
use log::{info, debug, trace};
use env_logger::{Builder, Env};


pub fn main() -> Result<(), Box<dyn Error>> {
    Builder::new().parse_env(Env::default().filter_or("DAISYRETRO_LOG", "debug")).init();

    let rom_path = env::args().skip(1).next().unwrap();
    info!("Loading {}...", rom_path);
    let rom = ines::InesRom::from_file(&rom_path)?;
    let mut famicom = Famicom::new_pinned();

    famicom.as_mut().load(&rom);
    famicom.as_mut().reset();
    //famicom.as_mut().cpu_mut().registers.PC = 0xc000;
    // famicom.unload();
    // famicom.loaded();

    //cpu.reset();

    // let cycle_budget = 3000;
    // cpu.run(cycle_budget);
    // Ok(())


    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();

    let window = video_subsystem.window("Daisy Retro - Demo", 256 * 3, 240 * 3)
        .position_centered()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas().build().unwrap();
    canvas.set_scale(3.0, 3.0)?;
    canvas.set_draw_color(Color::RGB(0, 0, 0));
    canvas.clear();
    canvas.present();

    let texture_creator = canvas.texture_creator();
    let mut texture = texture_creator.create_texture_target(PixelFormatEnum::RGB24, 256, 240).unwrap();

    let mut rng = rand::thread_rng();
    let mut frame2 = [0 as u8; 256 * 240 * 3];

    let mut event_pump = sdl_context.event_pump().unwrap();

    let mut key_map = HashMap::new();
    key_map.insert(Keycode::Up, Joypad::BUTTON_UP);
    key_map.insert(Keycode::W, Joypad::BUTTON_UP);
    key_map.insert(Keycode::Down, Joypad::BUTTON_DOWN);
    key_map.insert(Keycode::S, Joypad::BUTTON_DOWN);
    key_map.insert(Keycode::Left, Joypad::BUTTON_LEFT);
    key_map.insert(Keycode::A, Joypad::BUTTON_LEFT);
    key_map.insert(Keycode::Right, Joypad::BUTTON_RIGHT);
    key_map.insert(Keycode::D, Joypad::BUTTON_RIGHT);
    key_map.insert(Keycode::Return, Joypad::BUTTON_START);
    key_map.insert(Keycode::Return2, Joypad::BUTTON_START);
    key_map.insert(Keycode::LShift, Joypad::BUTTON_SELECT);
    key_map.insert(Keycode::RShift, Joypad::BUTTON_SELECT);
    key_map.insert(Keycode::Z, Joypad::BUTTON_B);
    key_map.insert(Keycode::Comma, Joypad::BUTTON_B);
    key_map.insert(Keycode::X, Joypad::BUTTON_A);
    key_map.insert(Keycode::Period, Joypad::BUTTON_A);


    let cpu_cycles_per_frame = 29780;
    let mut remain = 0;

    'running: loop {
        // i = (i + 1) % 255;
        // canvas.set_draw_color(Color::RGB(i, 64, 255 - i));
        // canvas.clear();
        let frame_start_instant = Instant::now();
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. } |
                Event::KeyDown { keycode: Some(Keycode::Escape), .. } => {
                    break 'running;
                }
                Event::KeyDown { keycode: Some(Keycode::Tab), .. } => {
                    famicom.as_mut().reset();
                }
                Event::KeyDown { keycode: Some(key), .. } => {
                    if let Some(k) = key_map.get(&key) {
                        famicom.as_mut().joypads_mut()[0].press(*k);
                    }
                }
                Event::KeyUp { keycode: Some(key), .. } => {
                    if let Some(k) = key_map.get(&key) {
                        famicom.as_mut().joypads_mut()[0].release(*k);
                    }
                }
                _ => {}
            }
        }
        // The rest of the game loop goes here...
        remain = famicom.as_mut().cpu_mut().run(cpu_cycles_per_frame * 2)?;
        if remain != 0 {
            trace!("Remain cycles {}", remain);
        }

        let frame = &famicom.as_mut().ppu_mut().screen;
        let mut update = true;
        if update {
            texture.update(None, frame, 256 * 3).unwrap();
            // famicom.as_mut().ppu_mut().render_screen(&mut frame2)?;
            // texture.update(None, &frame2, 256 * 3).unwrap();
            canvas.copy(&texture, None, None).unwrap();
            canvas.present();
        }

        let elapsed = frame_start_instant.elapsed();
        let duration_per_frame = Duration::new(0, 1_000_000_000 / 60);
        debug!("Frame takes {}/{}ms.", elapsed.as_millis(), duration_per_frame.as_millis());
        if elapsed < duration_per_frame {
            let to_sleep = duration_per_frame - elapsed;
            ::std::thread::sleep(to_sleep);
        }
    }
    Ok(())
}
