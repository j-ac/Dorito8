#![allow(dead_code)]
extern crate sdl2;
use sdl2::audio::{AudioCallback, AudioSpecDesired};
use sdl2::event::Event;
use sdl2::keyboard::Scancode;
use std::path::PathBuf;
use structopt::StructOpt;

const TWELVE_BITS: u16 = 0xFFF;
const EIGHT_BITS: u16 = 0xFF;
const FOUR_BITS: u16 = 0xF;
const MEM_SIZE: usize = 4096;
const SCREEN_WIDTH: u8 = 64;
const SCREEN_HEIGHT: u8 = 32;
const NUM_REGISTERS: usize = 16;
const PROGRAM_START_POINT: usize = 512;
const FONT: [u8; 80] = [
    0xF0, 0x90, 0x90, 0x90, 0xF0, // 0
    0x20, 0x60, 0x20, 0x20, 0x70, // 1
    0xF0, 0x10, 0xF0, 0x80, 0xF0, // 2
    0xF0, 0x10, 0xF0, 0x10, 0xF0, // 3
    0x90, 0x90, 0xF0, 0x10, 0x10, // 4
    0xF0, 0x80, 0xF0, 0x10, 0xF0, // 5
    0xF0, 0x80, 0xF0, 0x90, 0xF0, // 6
    0xF0, 0x10, 0x20, 0x40, 0x40, // 7
    0xF0, 0x90, 0xF0, 0x90, 0xF0, // 8
    0xF0, 0x90, 0xF0, 0x10, 0xF0, // 9
    0xF0, 0x90, 0xF0, 0x90, 0x90, // A
    0xE0, 0x90, 0xE0, 0x90, 0xE0, // B
    0xF0, 0x80, 0x80, 0x80, 0xF0, // C
    0xE0, 0x90, 0x90, 0x90, 0xE0, // D
    0xF0, 0x80, 0xF0, 0x80, 0xF0, // E
    0xF0, 0x80, 0xF0, 0x80, 0x80, // F
];
const DOWN_PRESS: bool = true;
const UP_PRESS: bool = false;

struct SquareWave {
    phase_inc: f32,
    phase: f32,
    volume: f32,
}

impl AudioCallback for SquareWave {
    type Channel = f32;

    fn callback(&mut self, out: &mut [f32]) {
        //generate a square wave
        for x in out.iter_mut() {
            *x = if self.phase <= 0.5 {
                self.volume
            } else {
                -self.volume
            };
            self.phase = (self.phase + self.phase_inc) % 1.0;
        }
    }
}

#[derive(StructOpt, Debug)]

struct Opt {
    /// Runs with debug info displayed
    #[structopt(short, long)]
    debug: bool,

    /// File to read.
    #[structopt(name = "FILE", parse(from_os_str))]
    file: PathBuf,

    #[structopt(short, long)]
    verbose: bool,

    /// The frequency instructions are executed at. Will be rounded to nearest 60Hz
    #[structopt(short, long)]
    frequency: u32,
    //TODO: replace the hardcoded 440.0 Hz with a command line parameter
    //#[structopt(short, long)]
    //audio_frequecy: f32,
}

pub fn main() {
    let mut opt = Opt::from_args();
    if opt.verbose {
        println!("input params: {:?}", opt);
    }

    opt.frequency = (((opt.frequency as f64 / 60.0).round()) as u32 * 60).max(60); //round to nearest multiple of 60 to aminimum of 60

    if opt.verbose {
        println!("Frequency rounded to: {:?}", opt.frequency)
    }

    let sys = hardware::System::new(opt.file, opt.frequency);

    run_game(sys);
}

fn run_game(mut sys: crate::hardware::System) {
    // ===Creating SDL2 boilerplate objects===
    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();

    let _window = video_subsystem
        .window("keyboard", 800, 600)
        .position_centered()
        .build()
        .map_err(|e| e.to_string())
        .unwrap();

    let mut events = sdl_context.event_pump().unwrap();

    // ===SDL2 audio===
    let audio_subsystem = sdl_context.audio().unwrap();
    let wave_spec = AudioSpecDesired {
        freq: Some(44_100),
        channels: Some(1),
        samples: None,
    };

    let device = audio_subsystem
        .open_playback(None, &wave_spec, |spec| SquareWave {
            phase_inc: 440.0 / spec.freq as f32,
            phase: 0.0,
            volume: -0.25,
        })
        .unwrap();
    //============================

    'gameloop: loop {
        // ===KEYBOARD INPUT WITH SDL2===
        for event in events.poll_iter() {
            match event {
                Event::Quit { .. } => break 'gameloop,
                Event::KeyDown {
                    scancode: Some(scancode),
                    ..
                } => key_press(&scancode, &mut sys.keyboard, DOWN_PRESS), //Call keypress telling it the key was pressed
                Event::KeyUp {
                    scancode: Some(scancode),
                    ..
                } => key_press(&scancode, &mut sys.keyboard, UP_PRESS), //Call keypress telling it the key was released

                _ => (),
            }
        }

        // ===AUDIO OUT WITH SDL2===
        // Toggle the emission of the square wave according to the hardware timers
        if sys.sound.time > 0 && device.status() == sdl2::audio::AudioStatus::Paused {
            device.resume();
        } else if sys.sound.time > 0 {
            assert!(device.status() == sdl2::audio::AudioStatus::Playing);
        } else {
            device.pause();
        }

        //GAMEPLAY LOOP
        let opcode: u16 = fetch(&mut sys.pc, &sys.mem); //Fetch one instruction
        let pc_increment = execute(opcode, &mut sys); //Execute instruction and record how it affects the program counter

        sys.pc.val += match pc_increment {
            ProgramCounterPolicy::NoIncrement => 0,
            ProgramCounterPolicy::StandardIncrement => 2,
            ProgramCounterPolicy::DoubleIncrement => 4,
        };

        sys.sync(); //Waits 1/frequency seconds and handles timers
    }
}

//Update the system's keyboard array with for a *single key* so it is 'true' if its pressed and 'false' otherwise
fn key_press(keypress: &sdl2::keyboard::Scancode, keys: &mut hardware::Keys, is_key_down: bool) {
    match keypress {
        Scancode::Num1 => keys.key[0] = is_key_down,
        Scancode::Num2 => keys.key[1] = is_key_down,
        Scancode::Num3 => keys.key[2] = is_key_down,
        Scancode::Num4 => keys.key[3] = is_key_down,

        Scancode::Q => keys.key[4] = is_key_down,
        Scancode::W => keys.key[5] = is_key_down,
        Scancode::E => keys.key[6] = is_key_down,
        Scancode::R => keys.key[7] = is_key_down,

        Scancode::A => keys.key[8] = is_key_down,
        Scancode::S => keys.key[9] = is_key_down,
        Scancode::D => keys.key[10] = is_key_down,
        Scancode::F => keys.key[11] = is_key_down,

        Scancode::Z => keys.key[12] = is_key_down,
        Scancode::X => keys.key[13] = is_key_down,
        Scancode::C => keys.key[14] = is_key_down,
        Scancode::V => keys.key[15] = is_key_down,

        _ => (),
    }
}

fn fetch(pc: &mut hardware::ProgramCounter, mem: &hardware::Memory) -> u16 {
    //build the opcode in two stages. 8 most significant bits, then 8 least significant
    let mut opcode: u16 = ((mem.indices[pc.val as usize]) as u16) << 8;
    opcode += mem.indices[(pc.val + 1) as usize] as u16;
    opcode
}

fn execute(opcode: u16, sys: &mut crate::hardware::System) -> ProgramCounterPolicy {
    let (nib1, nib2, nib3, nib4) = opcode_in_u16_to_nibbles(opcode);

    match (nib1, nib2, nib3, nib4) {
        (0x0, 0x0, 0xE, 0x0) => crate::opcode::cls(&mut sys.disp),
        (0x0, 0x0, 0xE, 0xE) => crate::opcode::ret(&mut sys.sp, &mut sys.pc),
        //(0x0, _, _, _) => crate::opcode::sys() TODO
        (0x1, _, _, _) => crate::opcode::jp(opcode % (TWELVE_BITS + 1), &mut sys.pc),
        (0x2, _, _, _) => crate::opcode::call(opcode % (TWELVE_BITS + 1), &mut sys.pc, &mut sys.sp),

        (0x3, _, _, _) => {
            assert!(nib2 <= 0xF); //nib2 contains a register number. there are only 16 registers.

            crate::opcode::se_const_vs_reg(
                (opcode % (EIGHT_BITS + 1)) as u8,
                sys.registers[nib2 as usize],
            )
        }
        (0x4, _, _, _) => {
            assert!(nib2 <= 0xF); //nib 2 contains a register number. There are only 16 registers.

            crate::opcode::sne_const_vs_reg(
                (opcode % (EIGHT_BITS + 1)) as u8,
                sys.registers[nib2 as usize],
            )
        }

        (0x5, _, _, 0x0) => {
            assert!(nib2 <= 0xF);
            assert!(nib3 <= 0xF); //nib 2 and 3 contain register numbers. There are only 16 registers

            crate::opcode::se_reg_vs_reg(sys.registers[nib2 as usize], sys.registers[nib3 as usize])
        }

        (0x6, _, _, _) => {
            assert!(nib2 <= 0xF); //nib2 contains a register number there are only 16 registers.

            crate::opcode::ld(
                &mut sys.registers[nib2 as usize],
                (opcode % (EIGHT_BITS + 1)) as u8, //gets last two nibbles as a u8
            )
        }

        (0x7, _, _, _) => {
            assert!(nib2 <= 0xF);

            crate::opcode::add(
                (opcode % (EIGHT_BITS + 1)) as u8,
                &mut sys.registers[nib2 as usize],
            )
        }

        (0x8, _, _, 0x0) => {
            assert!(nib2 <= 0xF);
            assert!(nib3 <= 0xF);

            crate::opcode::load_val_reg_to_reg(
                sys.registers[nib3 as usize],
                &mut sys.registers[nib2 as usize],
            )
        }

        (0x8, _, _, 0x1) => {
            assert!(nib2 <= 0xF);
            assert!(nib3 <= 0xF);

            crate::opcode::or(
                sys.registers[nib3 as usize],
                &mut sys.registers[nib2 as usize],
            )
        }

        (0x8, _, _, 0x2) => {
            assert!(nib2 <= 0xF);
            assert!(nib3 <= 0xF);

            crate::opcode::and(
                sys.registers[nib3 as usize],
                &mut sys.registers[nib2 as usize],
            )
        }

        (0x8, _, _, 0x3) => {
            assert!(nib2 <= 0xF);
            assert!(nib3 <= 0xF);

            crate::opcode::xor(
                sys.registers[nib3 as usize],
                &mut sys.registers[nib2 as usize],
            )
        }

        (0x8, _, _, 0x4) => {
            assert!(nib2 <= 0xF);
            assert!(nib3 <= 0xF);

            // Split the register array in two. Second array contains only the last element.
            // This is neccesarry to avoid rust borrowchecker rules preventing two mutable
            // references to a single array.
            let array_segments = sys.registers.split_at_mut(0xF);

            let arg1 = array_segments.0[nib3 as usize]; //Y
            let arg2 = &mut array_segments.0[nib2 as usize]; //X
            let arg3 = &mut array_segments.1[0]; //VF

            crate::opcode::add_two_regs(arg1, arg2, arg3)
        }

        (0x8, _, _, 0x5) => {
            assert!(nib2 <= 0xF);
            assert!(nib3 <= 0xF);

            // Split the register array in two. Second array contains only the last element.
            // This is neccesarry to avoid rust borrowchecker rules preventing two mutable
            // references to a single array.
            let array_segments = sys.registers.split_at_mut(0xF);

            let arg1 = array_segments.0[nib3 as usize]; //Y
            let arg2 = &mut array_segments.0[nib2 as usize]; //X
            let arg3 = &mut array_segments.1[0]; //VF

            crate::opcode::sub_two_regs(arg1, arg2, arg3)
        }

        (0x8, _, _, 0x6) => {
            assert!(nib2 <= 0xF);
            assert!(nib3 <= 0xF);

            // Split the register array in two. Second array contains only the last element.
            // This is neccesarry to avoid rust borrowchecker rules preventing two mutable
            // references to a single array.
            let array_segments = sys.registers.split_at_mut(0xF);

            let arg1 = &mut array_segments.0[nib2 as usize]; //X
            let arg2 = &mut array_segments.1[0]; //VF

            crate::opcode::shr(arg1, arg2)
        }

        (0x8, _, _, 0x7) => {
            assert!(nib2 <= 0xF);
            assert!(nib3 <= 0xF);

            // Split the register array in two. Second array contains only the last element.
            // This is neccesarry to avoid rust borrowchecker rules preventing two mutable
            // references to a single array.
            let array_segments = sys.registers.split_at_mut(0xF);

            let arg1 = array_segments.0[nib3 as usize]; //Y
            let arg2 = &mut array_segments.0[nib2 as usize]; //X
            let arg3 = &mut array_segments.1[0]; //VF

            crate::opcode::subn(arg1, arg2, arg3)
        }

        (0x8, _, _, 0xE) => {
            assert!(nib2 <= 0xF);

            let array_segments = sys.registers.split_at_mut(0xF);

            let arg1 = &mut array_segments.0[nib2 as usize]; //X
            let arg2 = &mut array_segments.1[0]; //VF

            crate::opcode::shl(arg1, arg2)
        }

        (0xA, _, _, _) => {
            let constant: u16 = nibbles_array_to_u16(&[nib2, nib3, nib4]);
            assert!(constant <= TWELVE_BITS);

            crate::opcode::ld_into_i(&mut sys.ireg, constant)
        }

        (0xB, _, _, _) => {
            let offset: u16 = nibbles_array_to_u16(&[nib2, nib3, nib4]);

            crate::opcode::jump_offset(sys.registers[0], offset, &mut sys.pc)
        }

        (0xC, _, _, _) => {
            assert!(nib2 <= 0xF);
            //Combine the last 2 nibbles into a u8 by successive shifts and adds
            let mut constant: u8 = nib3;
            constant <<= 4;
            constant += nib4;

            crate::opcode::rnd(&mut sys.registers[nib2 as usize], constant, &mut sys.rng)
        }

        (0xD, _, _, _) => {
            assert!(nib2 <= 0xF);
            assert!(nib3 <= 0xF);
            assert!(nib4 <= FOUR_BITS as u8); //ie is a nibble

            crate::opcode::draw(
                &mut sys.disp,
                &sys.mem,
                sys.registers[nib2 as usize],
                sys.registers[nib3 as usize],
                nib4,
                &mut sys.ireg,
                &mut sys.registers[0xF],
            )
        }

        (0xE, _, 0x9, 0xE) => {
            assert!(nib2 <= 0xF);

            crate::opcode::skip_if_key_pressed(sys.registers[nib2 as usize], &sys.keyboard)
        }

        (0xE, _, 0xA, 0x1) => {
            assert!(nib2 <= 0xF);

            crate::opcode::skip_if_key_not_pressed(sys.registers[nib2 as usize], &sys.keyboard)
        }

        (0xF, _, 0x0, 0x7) => {
            assert!(nib2 <= 0xF);

            crate::opcode::save_delay_timer_value(&sys.delay, &mut sys.registers[nib2 as usize])
        }

        //TODO
        (0xF, _, 0x0, 0xA) => {
            ProgramCounterPolicy::NoIncrement //PLACEHOLDER. REPLACE WITH FUNCTION CALL TO suspend_program_and_store_next_keypress
        }

        (0xF, _, 0x1, 0x5) => {
            assert!(nib2 <= 0xF);

            crate::opcode::set_delay_timer(&mut sys.delay, sys.registers[nib2 as usize])
        }

        (0xF, _, 0x1, 0xE) => {
            assert!(nib2 <= 0xF);

            crate::opcode::add_i_reg(&mut sys.ireg, sys.registers[nib2 as usize])
        }

        (0xF, _, 0x2, 0x9) => {
            assert!(nib2 <= 0xF);

            crate::opcode::load_hardcoded_sprite(&mut sys.ireg, sys.registers[nib2 as usize])
        }

        (0xF, _, 0x3, 0x3) => {
            assert!(nib2 <= 0xF);

            crate::opcode::save_decimal_value_to_memory(
                &sys.ireg,
                sys.registers[nib2 as usize],
                &mut sys.mem,
            )
        }

        (0xF, _, 0x5, 0x5) => {
            assert!(nib2 <= 0xF);

            crate::opcode::store_first_n_registers_in_memory(
                sys.registers,
                &sys.ireg,
                &mut sys.mem,
                nib2,
            )
        }

        (0xF, _, 0x6, 0x5) => {
            assert!(nib2 <= 0xF);

            crate::opcode::retrieve_first_n_registers_from_memory(
                &sys.mem,
                &mut sys.registers,
                &sys.ireg,
                nib2,
            )
        }

        (_, _, _, _) => panic!("Undefined opcode encountered: {:X}", opcode), //Print the opcode in hex
    }
}

// Express a 16 bit integer as a tuple of its 4 bit chunks.
fn opcode_in_u16_to_nibbles(opcode: u16) -> (u8, u8, u8, u8) {
    let first_nibble = (opcode >> 12) as u8;
    let second_nibble = ((opcode >> 8) % 0xF) as u8;
    let third_nibble = ((opcode >> 4) % 0xFF) as u8;
    let fourth_nibble = ((opcode) % 0xFFF) as u8;

    //no nibble datatype in rust. Using asserts to ensure they are only used as nibbles.
    assert!(first_nibble <= FOUR_BITS as u8);
    assert!(second_nibble <= FOUR_BITS as u8);
    assert!(third_nibble <= FOUR_BITS as u8);
    assert!(fourth_nibble <= FOUR_BITS as u8);

    (first_nibble, second_nibble, third_nibble, fourth_nibble)
}

//combine an array of nibbles into a single u16
fn nibbles_array_to_u16(nibbles: &[u8]) -> u16 {
    assert!(nibbles.len() <= 4); // More than 4 won't fit in a u16.
    let mut returner: u16 = 0;

    //Note for loops in rust are exclusive on the second value.
    for nibble in nibbles.iter() {
        //-1 so the last element is done outside of the for loop. Last shouldn't be shifted!
        assert!(nibble <= &(FOUR_BITS as u8)); //ie is a nibble

        returner += *nibble as u16;
        returner <<= 4;
    }

    returner += nibbles[(nibbles.len() - 1) as usize] as u16; //the one ignored by for loop.

    returner
}

//Return type of all opcodes
pub enum ProgramCounterPolicy {
    NoIncrement,
    StandardIncrement,
    DoubleIncrement,
}

mod hardware {

    use crate::FONT;
    use crate::MEM_SIZE;
    use crate::NUM_REGISTERS;
    use crate::PROGRAM_START_POINT;
    use rand::prelude::*;
    use std::fs::File;
    use std::io::prelude::*;
    use std::ops::Deref;
    use std::path::PathBuf;
    use std::time::*;

    #[derive(Default, Copy, Clone)]
    pub struct Register {
        pub val: u8,
    }

    pub struct IRegister {
        pub val: u16,
    }

    pub struct Keys {
        pub key: [bool; 16],
    }

    pub struct ProgramCounter {
        pub val: u16,
    }

    impl Deref for ProgramCounter {
        type Target = u16;

        fn deref(&self) -> &Self::Target {
            &self.val
        }
    }

    impl ProgramCounter {
        pub fn set(&mut self, input: u16) {
            self.val = input; // & 0b_0000_1111_1111_1111; // Some specs say this should only be 12 bits. Might be safer to truncate to 12 bits
        }
    }

    pub struct Memory {
        pub indices: [u8; MEM_SIZE],
    }

    impl Memory {
        fn load_in_hardcoded_sprites(&mut self) {
            self.indices[0..FONT.len()].copy_from_slice(&FONT);
        }

        fn load_rom(&mut self, rom: PathBuf) {
            let data = File::open(rom).unwrap();

            for (i, byte) in data.bytes().enumerate() {
                self.indices[PROGRAM_START_POINT + i] = byte.unwrap();
            }
        }
    }

    pub struct Display {
        pub data: [[bool; 64]; 32],
    }

    pub struct DelayTimer {
        pub time: u8,
    }

    pub struct SoundTimer {
        pub time: u8,
    }

    #[derive(Default)]
    pub struct Stack {
        stack: [u16; 16],
        pub sp: usize,
    }

    impl Stack {
        pub fn push(&mut self, pc: u16) {
            self.sp += 1;
            self.stack[self.sp] = pc;
        }

        pub fn pop(&mut self) -> u16 {
            let returner = self.stack[self.sp];
            self.sp -= 1;
            returner
        }
    }

    pub struct SyncData {
        pub num_cycles_until_timers_decrement: u16,
        pub frequency: u32,
        pub frequency_inverse: f64, //Unnecesarry?
    }

    pub struct System {
        pub registers: [Register; 16],
        pub mem: Memory,
        pub ireg: IRegister,
        pub pc: ProgramCounter,
        pub sp: Stack,
        pub keyboard: Keys,
        pub disp: Display,
        pub sound: SoundTimer,
        pub delay: DelayTimer,
        pub rng: rand::rngs::StdRng,
        pub sync_data: SyncData,
    }
    impl System {
        pub fn new(rom: PathBuf, frequency: u32) -> Self {
            let mut mem = Memory {
                indices: [0; MEM_SIZE],
            };
            mem.load_in_hardcoded_sprites();
            mem.load_rom(rom);

            Self {
                registers: [Register { val: 0 }; NUM_REGISTERS],
                mem,
                ireg: IRegister { val: 0 },
                pc: ProgramCounter { val: 0 },
                sp: Stack {
                    sp: 0,
                    stack: [0; 16],
                },
                keyboard: Keys { key: [false; 16] },
                disp: Display {
                    data: [[false; 64]; 32],
                },

                sound: SoundTimer { time: 0 },
                delay: DelayTimer { time: 0 },
                rng: rand::rngs::StdRng::from_entropy(),
                sync_data: SyncData {
                    num_cycles_until_timers_decrement: 0,
                    frequency,
                    frequency_inverse: frequency as f64 / 60.0,
                },
            }
        }

        // Waits a specified duration so the game runs at a given frequency.
        // Assumes the rest of the program has negligible execution time
        // If game runs clunky may need to be changed to wait for precise deadlines
        pub fn sync(&mut self) {
            assert!(self.sync_data.frequency % 60 == 0); //is a multiple of 60

            std::thread::sleep(Duration::new(1, 0) / self.sync_data.frequency as u32); // sleep for 1/frequency seconds

            //example: if frequency is 600Hz, then every 600/60 = 10 cycles the timers decrement
            if self.sync_data.num_cycles_until_timers_decrement == 0 {
                self.delay.time = self.delay.time.saturating_sub(1);
                self.sound.time = self.sound.time.saturating_sub(1);
                self.sync_data.num_cycles_until_timers_decrement =
                    (self.sync_data.frequency / 60) as u16
            } else {
                self.sync_data.num_cycles_until_timers_decrement -= 1;
            }
        }
    }
}

mod opcode {
    use crate::convert_u8_to_boolarr;
    use crate::ProgramCounterPolicy;
    use crate::DOWN_PRESS;
    use crate::EIGHT_BITS; //0xFF
    use crate::FOUR_BITS; //0xF
    use crate::MEM_SIZE;
    use crate::NUM_REGISTERS; //16
    use crate::SCREEN_HEIGHT;
    use crate::SCREEN_WIDTH;
    use crate::TWELVE_BITS; //0xFFF //4096

    use rand::prelude::*;

    // Jump to a routine
    // OPCODE: 0NNN
    pub fn sys() -> u16 {
        //TODO
        0
    }

    // Clear the display
    // OPCODE: 00E0
    pub fn cls(dis: &mut crate::hardware::Display) -> ProgramCounterPolicy {
        dis.data = [[false; 64]; 32];

        ProgramCounterPolicy::StandardIncrement
    }

    //Return from subroutine (pops the address from the top of the stack)
    // OPCODE: 00EE
    pub fn ret(
        stack: &mut crate::hardware::Stack,
        program_counter: &mut crate::hardware::ProgramCounter,
    ) -> ProgramCounterPolicy {
        program_counter.set(stack.pop());

        ProgramCounterPolicy::StandardIncrement //So that it does not call the subroutine infinitely
    }

    // Jump to location
    // OPCODE: 1NNN
    pub fn jp(
        input: u16,
        program_counter: &mut crate::hardware::ProgramCounter,
    ) -> ProgramCounterPolicy {
        assert!(input <= TWELVE_BITS); // value not exceeding 0xFFF
        program_counter.set(input);

        ProgramCounterPolicy::NoIncrement
    }

    // Call subroutine. Push PC to stack then jump to the given value
    // OPCODE: 2NNN
    pub fn call(
        input: u16,
        program_counter: &mut crate::hardware::ProgramCounter,
        stack: &mut crate::hardware::Stack,
    ) -> ProgramCounterPolicy {
        assert!(input <= TWELVE_BITS);

        stack.push(program_counter.val);
        program_counter.set(input);

        ProgramCounterPolicy::NoIncrement
    }

    // Skip if equal
    // OPCODE: 3XKK
    pub fn se_const_vs_reg(input: u8, register: crate::hardware::Register) -> ProgramCounterPolicy {
        if input == register.val {
            ProgramCounterPolicy::DoubleIncrement
        } else {
            ProgramCounterPolicy::StandardIncrement
        }
    }
    //skip if not equal
    // OPCODE: 4XKK
    pub fn sne_const_vs_reg(
        input: u8,
        register: crate::hardware::Register,
    ) -> ProgramCounterPolicy {
        if input != register.val {
            ProgramCounterPolicy::DoubleIncrement
        } else {
            ProgramCounterPolicy::StandardIncrement
        }
    }

    //skip if not equal
    // OPCODE: 5XY0
    pub fn se_reg_vs_reg(
        reg1: crate::hardware::Register,
        reg2: crate::hardware::Register,
    ) -> ProgramCounterPolicy {
        if reg1.val == reg2.val {
            ProgramCounterPolicy::DoubleIncrement
        } else {
            ProgramCounterPolicy::StandardIncrement
        }
    }

    // skip if not equal
    // OPCODE: 9XY0
    pub fn sne_reg_vs_reg(
        reg1: crate::hardware::Register,
        reg2: crate::hardware::Register,
    ) -> ProgramCounterPolicy {
        if reg1.val != reg2.val {
            ProgramCounterPolicy::DoubleIncrement
        } else {
            ProgramCounterPolicy::StandardIncrement
        }
    }

    // Load? seems like a bad naming convention...
    // OPCODE: 6XKK
    pub fn ld(register: &mut crate::hardware::Register, input: u8) -> ProgramCounterPolicy {
        register.val = input;

        ProgramCounterPolicy::StandardIncrement
    }

    // add constant to register
    // OPCODE: 7XKK
    pub fn add(input: u8, reg: &mut crate::hardware::Register) -> ProgramCounterPolicy {
        reg.val = reg.val.wrapping_add(input); //Documentation does not state an overflow policy. Will assume wrapping

        ProgramCounterPolicy::StandardIncrement
    }

    // Copy the value of reg2 into reg1
    // OPCODE: 8XY0
    pub fn load_val_reg_to_reg(
        reg2: crate::hardware::Register,
        reg1: &mut crate::hardware::Register,
    ) -> ProgramCounterPolicy {
        reg1.val = reg2.val;

        ProgramCounterPolicy::StandardIncrement
    }

    // bitwise or on two registers
    // OPCODE: 8XY1
    pub fn or(
        reg2: crate::hardware::Register,
        reg1: &mut crate::hardware::Register,
    ) -> ProgramCounterPolicy {
        reg1.val |= reg2.val;

        ProgramCounterPolicy::StandardIncrement
    }

    // bitwise and on two registers
    // OPCODE: 8XY2
    pub fn and(
        reg2: crate::hardware::Register,
        reg1: &mut crate::hardware::Register,
    ) -> ProgramCounterPolicy {
        reg1.val &= reg2.val;

        ProgramCounterPolicy::StandardIncrement
    }

    // bitwise xor on two registers
    // OPCODE: 8XY3
    pub fn xor(
        reg2: crate::hardware::Register,
        reg1: &mut crate::hardware::Register,
    ) -> ProgramCounterPolicy {
        reg1.val ^= reg2.val;

        ProgramCounterPolicy::StandardIncrement
    }

    // OPCODE: 8XY4
    pub fn add_two_regs(
        reg2: crate::hardware::Register,
        reg1: &mut crate::hardware::Register,
        overflow_flag: &mut crate::hardware::Register,
    ) -> ProgramCounterPolicy {
        //Differs from add() by the args and uses overflow detection
        if (reg1.val as u16 + reg2.val as u16) > EIGHT_BITS {
            overflow_flag.val = 1;
        }

        reg1.val = reg1.val.wrapping_add(reg2.val);

        ProgramCounterPolicy::StandardIncrement
    }

    // OPCODE: 8XY5
    pub fn sub_two_regs(
        reg2: crate::hardware::Register,
        reg1: &mut crate::hardware::Register,
        overflow_flag: &mut crate::hardware::Register,
    ) -> ProgramCounterPolicy {
        if reg1.val > reg2.val {
            overflow_flag.val = 1;
        } else {
            overflow_flag.val = 0; //This might be a failing of the documentation. THis function explicitly says to set to 0 but other similar functions don't specify
        }

        reg1.val = reg1.val.wrapping_sub(reg2.val);

        ProgramCounterPolicy::StandardIncrement
    }

    //shift register
    // OPCODE: 8XY6
    pub fn shr(
        reg: &mut crate::hardware::Register,
        truncate_register: &mut crate::hardware::Register,
    ) -> ProgramCounterPolicy {
        let (value, did_truncate) = reg.val.overflowing_shr(1);
        reg.val = value;
        truncate_register.val = did_truncate as u8;

        ProgramCounterPolicy::StandardIncrement
    }

    // Reversed operands order compared to sub()
    // OPCODE: 8XY7
    pub fn subn(
        reg2: crate::hardware::Register,
        reg1: &mut crate::hardware::Register,
        overflow_flag: &mut crate::hardware::Register,
    ) -> ProgramCounterPolicy {
        if reg2.val > reg1.val {
            overflow_flag.val = 1;
        } else {
            overflow_flag.val = 0;
        }

        reg1.val = reg2.val.wrapping_sub(reg1.val);

        ProgramCounterPolicy::StandardIncrement
    }

    // shift left.
    // Might result in bits "falling off" the edge
    //  OPCODE: 8XYE
    pub fn shl(
        reg: &mut crate::hardware::Register,
        overflow_flag: &mut crate::hardware::Register,
    ) -> ProgramCounterPolicy {
        let (val, did_overflow) = reg.val.overflowing_shl(1);
        reg.val = val;
        overflow_flag.val = did_overflow as u8;

        ProgramCounterPolicy::StandardIncrement
    }

    // Load a constant into the IRegister
    // OPCODE: ANNN
    pub fn ld_into_i(i_reg: &mut crate::hardware::IRegister, input: u16) -> ProgramCounterPolicy {
        assert!(input <= TWELVE_BITS);
        i_reg.val = input;

        ProgramCounterPolicy::StandardIncrement
    }

    // jumps to Register 0's value + offset
    // OPCODE: BNNN
    pub fn jump_offset(
        reg_zero: crate::hardware::Register,
        input: u16,
        program_counter: &mut crate::hardware::ProgramCounter,
    ) -> ProgramCounterPolicy {
        assert!(input <= TWELVE_BITS);
        program_counter.val = reg_zero.val as u16 + input;

        ProgramCounterPolicy::NoIncrement
    }

    // random number generator. Binary AND against an input.
    // OPCODE: CXKK
    pub fn rnd<T: RngCore>(
        dest_reg: &mut crate::hardware::Register,
        input: u8,
        rng: &mut T,
    ) -> ProgramCounterPolicy {
        let random: u8 = rng.gen();
        dest_reg.val = random & input;

        ProgramCounterPolicy::StandardIncrement
    }

    // OPCODE: DXYN
    pub fn draw(
        dis: &mut crate::hardware::Display,
        mem: &crate::hardware::Memory,
        reg_x: crate::hardware::Register,
        reg_y: crate::hardware::Register,
        nibble: u8,
        memory_position: &mut crate::hardware::IRegister,
        collision_detect_register: &mut crate::hardware::Register, // indicates collision with an existing sprite
    ) -> ProgramCounterPolicy {
        assert!(nibble <= FOUR_BITS as u8);
        assert!(memory_position.val + ((nibble - 1) as u16) <= MEM_SIZE as u16);

        for y in 0..nibble {
            //nible indicates the number of bytes (layers)
            let byte = mem.indices[(memory_position.val + y as u16) as usize]; //load in a single layer of the sprite
            let byte_as_bool_array = convert_u8_to_boolarr(byte);

            for x in 0..8 {
                let x_coordinate: usize = ((reg_x.val + x) % SCREEN_WIDTH) as usize;
                let y_coordinate: usize = ((reg_y.val + y) % SCREEN_HEIGHT) as usize;

                //if the display has a pixel here. (ie a lit pixel is on screen AND in the sprite)
                if (dis.data[x_coordinate][y_coordinate]) && (byte_as_bool_array[x as usize]) {
                    collision_detect_register.val = 1;
                }

                dis.data[x_coordinate][y_coordinate] ^= byte_as_bool_array[x as usize];
                //insert the sprite by XORing
            }
        }
        ProgramCounterPolicy::StandardIncrement
    }

    // OPCODE: EX9E
    pub fn skip_if_key_pressed(
        reg: crate::hardware::Register,
        keys: &crate::hardware::Keys,
    ) -> ProgramCounterPolicy {
        if keys.key[reg.val as usize] {
            ProgramCounterPolicy::DoubleIncrement
        } else {
            ProgramCounterPolicy::StandardIncrement
        }
    }

    // OPCODE: EXA1
    pub fn skip_if_key_not_pressed(
        reg: crate::hardware::Register,
        keys: &crate::hardware::Keys,
    ) -> ProgramCounterPolicy {
        if !keys.key[reg.val as usize] {
            ProgramCounterPolicy::DoubleIncrement
        } else {
            ProgramCounterPolicy::StandardIncrement
        }
    }

    //save the delay timer value into a register
    //opcode: FX07
    pub fn save_delay_timer_value(
        timer: &crate::hardware::DelayTimer,
        reg: &mut crate::hardware::Register,
    ) -> ProgramCounterPolicy {
        reg.val = timer.time;

        ProgramCounterPolicy::StandardIncrement
    }

    //suspend execution until a key is pressed and record the key that was pressed.
    //opcode: FX0A

    pub fn suspend_program_and_store_next_keypress(
        keys: crate::hardware::Keys,
        reg: &mut crate::hardware::Register,
    ) -> ProgramCounterPolicy {
        for i in 0..keys.key.len() {
            if keys.key[i] == DOWN_PRESS {
                reg.val = i as u8; //Record the key
                return ProgramCounterPolicy::StandardIncrement; //
            }
        }
        ProgramCounterPolicy::NoIncrement //Nothing was pressed, try again
    }

    //Set the value of the delay timer
    //opcode FX15
    pub fn set_delay_timer(
        timer: &mut crate::hardware::DelayTimer,
        reg: crate::hardware::Register,
    ) -> ProgramCounterPolicy {
        timer.time = reg.val;

        ProgramCounterPolicy::StandardIncrement
    }

    //aubd a value to the i_reg from another register
    //opcode: FX1E
    pub fn add_i_reg(
        i_reg: &mut crate::hardware::IRegister,
        reg: crate::hardware::Register,
    ) -> ProgramCounterPolicy {
        i_reg.val = i_reg.val.wrapping_add(reg.val as u16);

        ProgramCounterPolicy::StandardIncrement
    }

    // set the I register to the memory address containing the sprite representing the numeral
    // stored in the register. Doesn't actally load anything yet.
    // opcode: FX29
    pub fn load_hardcoded_sprite(
        i_reg: &mut crate::hardware::IRegister,
        digit: crate::hardware::Register,
    ) -> ProgramCounterPolicy {
        assert!(digit.val <= 0xF); //Documentation doesn't specify what to do in this instance
        i_reg.val = (digit.val * 5) as u16; //Sprites are 5 bytes

        ProgramCounterPolicy::StandardIncrement
    }

    //converts a binary value in a register to a decimal value then saves its digits to memory
    //sequentially
    //example: 1111 1111 = 255. Saves in three sequential memory addresses '2' '5' '5'
    //opcode: FX33
    pub fn save_decimal_value_to_memory(
        i_reg: &crate::hardware::IRegister,
        reg: crate::hardware::Register,
        mem: &mut crate::hardware::Memory,
    ) -> ProgramCounterPolicy {
        assert!(i_reg.val + 2 <= MEM_SIZE as u16);

        let hundreds_place = reg.val / 100; //Rust rounds down the renmainder
        let tens_place = (reg.val % 100) / 10;
        let ones_place = reg.val % 10;

        let i = i_reg.val as usize; //for conciseness
        mem.indices[i] = hundreds_place;
        mem.indices[i + 1] = tens_place;
        mem.indices[i + 2] = ones_place;

        ProgramCounterPolicy::StandardIncrement
    }

    // Store the first N registers to memory sequentially, where N is the value of the input
    // register.
    // OPCODE: FX55
    pub fn store_first_n_registers_in_memory(
        reg_array: [crate::hardware::Register; NUM_REGISTERS],
        i_reg: &crate::hardware::IRegister,
        mem: &mut crate::hardware::Memory,
        num_to_store: u8, //Rather than passing in the register which the borrowchecker will not like in combination with reg_array!
    ) -> ProgramCounterPolicy {
        assert!(i_reg.val + (num_to_store) as u16 <= MEM_SIZE as u16); //enough space to store the data
        assert!(num_to_store <= NUM_REGISTERS as u8); //Spec does not define what would occur in this scenario.

        for register_number in 0..num_to_store {
            mem.indices[(i_reg.val + register_number as u16) as usize] =
                reg_array[register_number as usize].val;
        }

        ProgramCounterPolicy::StandardIncrement
    }

    // OPCODE: FX65
    // Retrieve from memory and save them in registers
    pub fn retrieve_first_n_registers_from_memory(
        mem: &crate::hardware::Memory,
        reg_array: &mut [crate::hardware::Register; NUM_REGISTERS],
        i_reg: &crate::hardware::IRegister,
        num_regs: u8, //Comes from a register but to avoid annoying the borrowchecker we do this
    ) -> ProgramCounterPolicy {
        assert!(i_reg.val + ((num_regs) as u16) <= MEM_SIZE as u16);
        assert!(num_regs <= NUM_REGISTERS as u8);

        for register_number in 0..num_regs {
            reg_array[register_number as usize].val =
                mem.indices[(i_reg.val + register_number as u16) as usize];
        }
        ProgramCounterPolicy::StandardIncrement
    }
}

//Represent a u8 as a boolean array of its binary representation
fn convert_u8_to_boolarr(byte: u8) -> [bool; 8] {
    let mut boolarr: [bool; 8] = [false; 8];

    for (i, data) in boolarr.iter_mut().enumerate() {
        *data = byte & (0b1000_0000 >> i) != 0;
    }

    /*
        for i in 0..8 {
            boolarr[i] = byte & (0b1000_0000 >> i) != 0;
        }
    */
    boolarr
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_convert_u8_to_boolarr() {
        let ret = convert_u8_to_boolarr(0b1010_1010);
        assert_eq!(ret, [true, false, true, false, true, false, true, false]);

        let ret = convert_u8_to_boolarr(0b0000_0000);
        assert_eq!(
            ret,
            [false, false, false, false, false, false, false, false]
        );

        let ret = convert_u8_to_boolarr(0b1111_1111);
        assert_eq!(ret, [true, true, true, true, true, true, true, true]);

        let ret = convert_u8_to_boolarr(0b1000_0001);
        assert_eq!(ret, [true, false, false, false, false, false, false, true]);
    }
}
