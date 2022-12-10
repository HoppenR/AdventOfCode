use std::io::{self, Error, Read, Write};

enum InstrType {
    Noop,
    Addx,
}

struct Instruction {
    cmd: InstrType,
    arg: i32,
    cycle_count: u32,
}

fn run(instructions: &Vec<Instruction>) -> i32 {
    let mut signal_sum: i32 = 0;
    let mut pc: i32 = 0;
    let mut x: i32 = 1;
    for instr in instructions {
        for _ in 0..instr.cycle_count {
            pc += 1;
            if pc % 40 == 20 {
                signal_sum += pc * x
            }
        }
        match &instr.cmd {
            InstrType::Addx => {
                x += instr.arg;
            }
            InstrType::Noop => {}
        }
    }
    return signal_sum;
}

fn draw(instructions: &Vec<Instruction>) -> String {
    let mut ret: String = String::new();
    let mut pc: i32 = 0;
    let mut x: i32 = 1;
    for instr in instructions {
        for _ in 0..instr.cycle_count {
            if pc % 40 == 0 {
                ret += "\n";
                pc = 0;
            }
            if i32::abs(x - pc) <= 1 {
                ret += "#";
            } else {
                ret += ".";
            }
            pc += 1;
        }
        match &instr.cmd {
            InstrType::Addx => {
                x += instr.arg;
            }
            InstrType::Noop => {}
        }
    }
    ret.pop();
    ret.pop();
    return ret;
}

fn main() -> Result<(), Error> {
    let mut input: String = String::new();
    io::stdin().read_to_string(&mut input).unwrap();

    let instructions: Vec<Instruction> = input
        .split("\n")
        .map(|line| {
            let (cmd_cstr, arg_str): (&str, &str) = line.split_once(" ").unwrap_or(("noop", "0"));
            match cmd_cstr {
                "addx" => {
                    return Instruction {
                        cycle_count: 2,
                        cmd: InstrType::Addx,
                        arg: arg_str.parse().unwrap(),
                    }
                }
                "noop" => {
                    return Instruction {
                        cycle_count: 1,
                        cmd: InstrType::Noop,
                        arg: arg_str.parse().unwrap(),
                    }
                }
                _ => unreachable!(),
            }
        })
        .collect();

    writeln!(io::stdout(), "p1: {}", run(&instructions)).unwrap();
    writeln!(io::stdout(), "p2: {}", draw(&instructions)).unwrap();
    Ok(())
}
