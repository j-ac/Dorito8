use std::io::Read;
use std::io::Seek;
use std::path::PathBuf;
use structopt::StructOpt;

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
    struct Register {
        val: u8,
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

    struct Memory {
        memory: [u8; 4096],
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

    // Jump to a routine
    fn sys() -> u16 {
        //TODO
        return 0;
    }

    // Clear the display
    fn cls() { //TODO
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
        assert!(input <= 0xFFF); // value not exceeding 12 bits
        program_counter.set(input);
    }

    // Call subroutine. Push PC to stack then jump to the given value
    fn call(
        input: u16,
        program_counter: &mut crate::hardware::ProgramCounter,
        stack: &mut crate::hardware::Stack,
    ) {
        assert!(input <= 0xFFF); // Not exceeding 12 bits

        stack.push(program_counter.val);
        program_counter.set(input);
    }
}

fn run_game<R: Read + Seek>(rom: &R) {}
