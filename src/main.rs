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

pub fn main() -> Result<(), Box<dyn Error>> {
    let rom_path = env::args().skip(1).next().unwrap();
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
    let mut frame = [0 as u8; 256 * 240 * 3];

    let mut event_pump = sdl_context.event_pump().unwrap();

    let cpu_cycles_per_frame = 29780;

    'running: loop {
        // i = (i + 1) % 255;
        // canvas.set_draw_color(Color::RGB(i, 64, 255 - i));
        // canvas.clear();
        let frame_start_instant = Instant::now();
        famicom.as_mut().joypads_mut()[0].set(0);
        famicom.as_mut().joypads_mut()[1].set(0);
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. } |
                Event::KeyDown { keycode: Some(Keycode::Escape), .. } => {
                    break 'running;
                }
                Event::KeyDown { keycode: Some(Keycode::Up), .. } |
                Event::KeyDown { keycode: Some(Keycode::W), .. } => {
                    famicom.as_mut().joypads_mut()[0].press(Joypad::BUTTON_UP);
                }
                Event::KeyDown { keycode: Some(Keycode::Down), .. } |
                Event::KeyDown { keycode: Some(Keycode::S), .. } => {
                    famicom.as_mut().joypads_mut()[0].press(Joypad::BUTTON_DOWN);
                }
                Event::KeyDown { keycode: Some(Keycode::Left), .. } |
                Event::KeyDown { keycode: Some(Keycode::A), .. } => {
                    famicom.as_mut().joypads_mut()[0].press(Joypad::BUTTON_LEFT);
                }
                Event::KeyDown { keycode: Some(Keycode::Right), .. } |
                Event::KeyDown { keycode: Some(Keycode::D), .. } => {
                    famicom.as_mut().joypads_mut()[0].press(Joypad::BUTTON_RIGHT);
                }
                Event::KeyDown { keycode: Some(Keycode::Space), .. } |
                Event::KeyDown { keycode: Some(Keycode::Return), .. } => {
                    famicom.as_mut().joypads_mut()[0].press(Joypad::BUTTON_START);
                }
                Event::KeyDown { keycode: Some(Keycode::LShift), .. } |
                Event::KeyDown { keycode: Some(Keycode::RShift), .. } => {
                    famicom.as_mut().joypads_mut()[0].press(Joypad::BUTTON_SELECT);
                }
                Event::KeyDown { keycode: Some(Keycode::Z), .. } => {
                    famicom.as_mut().joypads_mut()[0].press(Joypad::BUTTON_B);
                }
                Event::KeyDown { keycode: Some(Keycode::X), .. } => {
                    famicom.as_mut().joypads_mut()[0].press(Joypad::BUTTON_A);
                }
                _ => {}
            }
        }
        // The rest of the game loop goes here...
        let remain = famicom.as_mut().cpu_mut().run(cpu_cycles_per_frame)?;
        if remain != 0 {
            println!("Remain cycles {}", remain);
        }
        famicom.as_mut().ppu_mut().render_screen(&mut frame)?;
        // for tile_y in 0..30usize {
        //     for tile_x in 0..32usize {
        //         let tile_id = tile_y * 32 + tile_x;
        //         famicom.as_mut().ppu_mut().render_tile(0, tile_id as u8, &mut frame, tile_x * 8, tile_y * 8)?;
        //     }
        // }

        let mut update = true;
        if update {
            texture.update(None, &frame, 256 * 3).unwrap();
            canvas.copy(&texture, None, None).unwrap();
            canvas.present();
        }

        let elapsed = frame_start_instant.elapsed();
        let duration_per_frame = Duration::new(0, 1_000_000_000 / 60);
        println!("Frame takes {}/{}ms.", elapsed.as_millis(), duration_per_frame.as_millis());
        if elapsed < duration_per_frame {
            let to_sleep = duration_per_frame - elapsed;
            ::std::thread::sleep(to_sleep);
        }
    }
    Ok(())
}
