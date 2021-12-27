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
    struct AddressRegister {
        val: u16, //Only use least significant 12 bits.
    }

    impl Deref for AddressRegister {
        type Target = u16;

        fn deref(&self) -> &Self::Target {
            &self.val
        }
    }

    impl AddressRegister {
        fn set(&mut self, input: u16) {
            self.val = input; // & 0b_0000_1111_1111_1111; // Some specs say this should only be 12 bytes. Might be safer to truncate to 12 bits
        }
    }

    struct Memory {
        memory: [u8; 4096],
    }

    #[derive(Default)]
    struct Stack {
        stack: [u16; 16],
        sp: usize,
    }

    impl Stack {
        fn push(&mut self, pc: u16) {
            self.stack[self.sp] = pc;
            self.sp += 1;
        }

        fn pop(&mut self) -> u16 {
            self.sp -= 1;
            self.stack[self.sp]
        }
    }
}

fn run_game<R: Read + Seek>(rom: &R) {}
