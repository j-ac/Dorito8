#![allow(dead_code)]
use std::io::Read;
use std::io::Seek;
use std::path::PathBuf;
use structopt::StructOpt;

const TWELVE_BITS: u16 = 0xFFF;
const EIGHT_BITS: u16 = 0xFF;
const FOUR_BITS: u16 = 0xF;
const MEM_SIZE: usize = 4096;
const SCREEN_WIDTH: u8 = 64;
const SCREEN_HEIGHT: u8 = 32;

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
}

fn main() {
    let opt = Opt::from_args();
    if opt.verbose {
        println!("input params: {:?}", opt);
    }

    let sys = hardware::System::new();
}

mod hardware {

    use crate::MEM_SIZE;
    use std::ops::Deref;

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
        sp: usize,
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

    pub struct System {
        registers: [Register; 16],
        mem: Memory,
        ireg: IRegister,
        pc: ProgramCounter,
        sp: Stack,
        keyboard: Keys,
        disp: Display,
    }
    impl System {
        pub fn new() -> Self {
            Self {
                registers: [Register { val: 0 }; 16],
                mem: Memory {
                    indices: [0; MEM_SIZE],
                },
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
            }
        }
    }
}

mod opcode {
    use crate::EIGHT_BITS; //0xFF
    use crate::FOUR_BITS; //0xF
    use crate::MEM_SIZE;
    use crate::SCREEN_HEIGHT;
    use crate::SCREEN_WIDTH;
    use crate::TWELVE_BITS; //0xFFF //4096

    use crate::convert_u8_to_boolarr;

    use rand::prelude::*;

    // Jump to a routine
    fn sys() -> u16 {
        //TODO
        return 0;
    }

    // Clear the display
    fn cls(dis: &mut crate::hardware::Display) {
        dis.data = [[false; 64]; 32];
    }

    //Return from subroutine (pops the address from the top of the stack)
    fn ret(
        stack: &mut crate::hardware::Stack,
        program_counter: &mut crate::hardware::ProgramCounter,
    ) {
        program_counter.set(stack.pop());
    }

    // Jump to location
    fn jp(input: u16, program_counter: &mut crate::hardware::ProgramCounter) {
        assert!(input <= TWELVE_BITS); // value not exceeding 0xFFF
        program_counter.set(input);
    }

    // Call subroutine. Push PC to stack then jump to the given value
    fn call(
        input: u16,
        program_counter: &mut crate::hardware::ProgramCounter,
        stack: &mut crate::hardware::Stack,
    ) {
        assert!(input <= TWELVE_BITS);
        stack.push(program_counter.val);
        program_counter.set(input);
    }

    // Skip if equal
    fn se_const_vs_reg(
        input: u8,
        register: crate::hardware::Register,
        program_counter: &mut crate::hardware::ProgramCounter,
    ) {
        if input == register.val {
            program_counter.val += 2; //Skip one instruction
        }
    }
    //skip if not equal
    fn sne_const_vs_reg(
        input: u8,
        register: crate::hardware::Register,
        program_counter: &mut crate::hardware::ProgramCounter,
    ) {
        if input != register.val {
            program_counter.val += 2;
        } //Skip one instruction
    }

    fn se_reg_vs_reg(
        reg1: crate::hardware::Register,
        reg2: crate::hardware::Register,
        program_counter: &mut crate::hardware::ProgramCounter,
    ) {
        if reg1.val == reg2.val {
            program_counter.val += 2;
        }
    }

    fn sne_reg_vs_reg(
        reg1: crate::hardware::Register,
        reg2: crate::hardware::Register,
        program_counter: &mut crate::hardware::ProgramCounter,
    ) {
        if reg1.val != reg2.val {
            program_counter.val += 2;
        }
    }

    // Load? seems like a bad naming convention...
    fn ld(register: &mut crate::hardware::Register, input: u8) {
        register.val = input
    }

    // add constant to register
    fn add(input: u8, reg: &mut crate::hardware::Register) {
        reg.val = reg.val.wrapping_add(input); //Documentation does not state an overflow policy. Will assume wrapping
    }

    // bitwise or on two registers
    fn or(reg1: &mut crate::hardware::Register, reg2: crate::hardware::Register) {
        reg1.val = reg1.val | reg2.val;
    }

    // bitwise and on two registers
    fn and(reg1: &mut crate::hardware::Register, reg2: crate::hardware::Register) {
        reg1.val = reg1.val & reg2.val;
    }

    // bitwise xor on two registers
    fn xor(reg1: &mut crate::hardware::Register, reg2: crate::hardware::Register) {
        reg1.val = reg1.val ^ reg2.val;
    }

    fn add_two_regs(
        reg1: &mut crate::hardware::Register,
        reg2: crate::hardware::Register,
        overflow_flag: &mut crate::hardware::Register,
    ) {
        //Differs from add() by the args and uses overflow detection
        if (reg1.val as u16 + reg2.val as u16) > EIGHT_BITS {
            overflow_flag.val = 1;
        }

        reg1.val = reg1.val.wrapping_add(reg2.val);
    }

    fn sub(
        reg1: &mut crate::hardware::Register,
        reg2: crate::hardware::Register,
        overflow_flag: &mut crate::hardware::Register,
    ) {
        if reg1.val > reg2.val {
            overflow_flag.val = 1;
        } else {
            overflow_flag.val = 0; //This might be a failing of the documentation. THis function explicitly says to set to 0 but other similar functions don't specify
        }

        reg1.val = reg1.val.wrapping_sub(reg2.val); //Overflow policy not stated, assuming wrapping subtraction.
    }

    //shift register
    fn shr(
        reg: &mut crate::hardware::Register,
        fraction_truncated_flag: &mut crate::hardware::Register,
    ) {
        let reg_is_odd: bool = reg.val % 2 == 1;
        if reg_is_odd {
            fraction_truncated_flag.val = 1;
        }
        reg.val = reg.val / 2;
    }

    // Reversed operands order compared to sub()
    fn subn(
        reg1: &mut crate::hardware::Register,
        reg2: &mut crate::hardware::Register,
        overflow_flag: &mut crate::hardware::Register,
    ) {
        if reg2.val > reg1.val {
            overflow_flag.val = 1;
        } else {
            overflow_flag.val = 0;
        }

        reg1.val = reg2.val - reg1.val;
    }

    // shift left.
    // Might result in bits "falling off" the edge
    fn shl(reg: &mut crate::hardware::Register, overflow_flag: &mut crate::hardware::Register) {
        if reg.val >= 0b_1000_0000 {
            //ie if the most significant bit is 1
            overflow_flag.val = 1;
        }
        reg.val *= 2;
    }

    // Load a constant into the IRegister
    fn ld_into_i(i_reg: &mut crate::hardware::IRegister, input: u16) {
        assert!(input <= TWELVE_BITS);
        i_reg.val = input;
    }

    //jumps to Register 0's value + offset
    fn jump_off_set(
        reg_zero: crate::hardware::Register,
        input: u16,
        program_counter: &mut crate::hardware::ProgramCounter,
    ) {
        assert!(input <= TWELVE_BITS);
        program_counter.val = reg_zero.val as u16 + input;
    }

    // random number generator. Binary AND against an input.
    fn rnd<T: RngCore>(dest_reg: &mut crate::hardware::Register, input: u8, rng: &mut T) {
        let random: u8 = rng.gen();
        dest_reg.val = random & input;
    }

    fn draw(
        dis: &mut crate::hardware::Display,
        mem: crate::hardware::Memory,
        reg_x: crate::hardware::Register,
        reg_y: crate::hardware::Register,
        nibble: u8,
        memory_position: &mut crate::hardware::IRegister,
        collision_detect_register: &mut crate::hardware::Register, // indicates collision with an existing sprite
    ) {
        assert!(nibble <= FOUR_BITS as u8);

        for y in 0..nibble {
            //nible indicates the number of bytes (layers)
            let byte = mem.indices[(memory_position.val + y as u16) as usize]; //load in a single layer of the sprite
            let byte_as_bool_array = convert_u8_to_boolarr(byte);

            for x in 0..8 {
                let x_coordinate: usize = ((reg_x.val + x) % SCREEN_WIDTH) as usize;
                let y_coordinate: usize = ((reg_y.val + y) % SCREEN_HEIGHT) as usize;

                if (dis.data[x_coordinate][y_coordinate] == true) //ie if the xor will delete a pixel
                    && (byte_as_bool_array[x as usize] == true)
                {
                    collision_detect_register.val = 1;
                }

                dis.data[x_coordinate][y_coordinate] =
                    dis.data[x_coordinate][y_coordinate] ^ byte_as_bool_array[x as usize];
                //insert the sprite by XORing
            }
        }
    }

    fn skip_if_key_pressed(
        reg: crate::hardware::Register,
        program_counter: &mut crate::hardware::ProgramCounter,
        keys: crate::hardware::Keys,
    ) {
        if keys.key[reg.val as usize] {
            program_counter.val += 2;
        }
    }

    fn skip_if_key_not_pressed(
        reg: crate::hardware::Register,
        program_counter: &mut crate::hardware::ProgramCounter,
        keys: crate::hardware::Keys,
    ) {
        if keys.key[reg.val as usize] {
            program_counter.val += 2;
        }
    }

    //save the delay timer value into a register
    fn save_delay_timer_value(
        timer: crate::hardware::DelayTimer,
        reg: &mut crate::hardware::Register,
    ) {
        reg.val = timer.time;
    }

    fn suspend_program_and_store_next_keypress() -> u8 {
        //TODO
        0
    }

    fn set_delay_timer(timer: &mut crate::hardware::DelayTimer, reg: crate::hardware::Register) {
        timer.time = reg.val;
    }

    fn add_i_reg(i_reg: &mut crate::hardware::IRegister, reg: crate::hardware::Register) {
        i_reg.val = i_reg.val + reg.val as u16;
    }

    // set the I register to the memory address containing the sprite representing the numeral
    // stored in the register.
    fn load_hardcoded_sprite() {
        //TODO (haven't hardcoded the sprites yet)
    }

    //converts a binary value in a register to a decimal value then saves its digits to memory
    //sequentially
    //example: 1111 1111 = 255. Saves in three sequential memory addresses '2' '5' '5'
    fn save_decimal_value_to_memory(
        i_reg: crate::hardware::IRegister,
        reg: crate::hardware::Register,
        mem: &mut crate::hardware::Memory,
    ) {
        let hundreds_place = reg.val / 100; //Rust rounds down the remainder
        let tens_place = (reg.val % 100) / 10;
        let ones_place = reg.val % 10;

        let i = i_reg.val as usize; //for conciseness
        mem.indices[i] = hundreds_place;
        mem.indices[i + 1] = tens_place;
        mem.indices[i + 2] = ones_place;
    }

    //Store the first N registers to memory sequentially, where N is the value of the input
    //register.
    fn store_first_n_registers_in_memory(
        reg_array: [crate::hardware::Register; 16],
        i_reg: crate::hardware::IRegister,
        mem: &mut crate::hardware::Memory,
        reg: crate::hardware::Register,
    ) {
        for register_number in 0..reg.val {
            mem.indices[(i_reg.val + register_number as u16) as usize] =
                reg_array[register_number as usize].val;
        }
    }

    fn retrieve_first_n_registers_from_memory() {
        //TODO
    }
}

fn convert_u8_to_boolarr(byte: u8) -> [bool; 8] {
    let mut boolarr: [bool; 8] = [false; 8];

    for i in 0..8 {
        let mut byte = byte;
        byte = byte >> i;
        boolarr[i] = (byte >> i) == 0b1;
    }

    boolarr
}

fn run_game<R: Read + Seek>(rom: &R) {}
