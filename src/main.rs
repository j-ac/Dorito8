use std::io::Read;
use std::io::Seek;
use std::path::PathBuf;
use structopt::StructOpt;

const TWELVE_BITS: u16 = 0xFFF;

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
}

mod hardware {
    use std::ops::Deref;

    #[derive(Default)]
    pub struct Register {
        pub val: u8,
    }

    #[derive(Default)]
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
        pub memory: [u8; 4096],
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
}

mod opcode {

    use crate::TWELVE_BITS;

    // Jump to a routine
    fn sys() -> u16 {
        //TODO
        return 0;
    }

    // Clear the display
    fn cls(memory: &mut crate::hardware::Memory) { //TODO
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
        register.val = input;
    }
}

fn run_game<R: Read + Seek>(rom: &R) {}
