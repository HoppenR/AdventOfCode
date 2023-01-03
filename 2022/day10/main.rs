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
    return ret;
}

fn main() -> Result<(), Error> {
    let mut input: String = String::new();
    io::stdin().read_to_string(&mut input)?;
    let instructions: Vec<Instruction> = parse(input.trim());
    writeln!(io::stdout(), "p1: {}", run(&instructions))?;
    writeln!(io::stdout(), "p2: {}", draw(&instructions))?;
    Ok(())
}

fn parse(input: &str) -> Vec<Instruction> {
    return input
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
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE: &str = "\
addx 15\naddx -11\naddx 6\naddx -3\naddx 5\naddx -1\naddx -8\naddx 13\naddx 4\nnoop\naddx -1
addx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx -35\naddx 1\naddx 24
addx -19\naddx 1\naddx 16\naddx -11\nnoop\nnoop\naddx 21\naddx -15\nnoop\nnoop\naddx -3
addx 9\naddx 1\naddx -3\naddx 8\naddx 1\naddx 5\nnoop\nnoop\nnoop\nnoop\nnoop
addx -36\nnoop\naddx 1\naddx 7\nnoop\nnoop\nnoop\naddx 2\naddx 6\nnoop\nnoop
noop\nnoop\nnoop\naddx 1\nnoop\nnoop\naddx 7\naddx 1\nnoop\naddx -13\naddx 13
addx 7\nnoop\naddx 1\naddx -33\nnoop\nnoop\nnoop\naddx 2\nnoop\nnoop\nnoop
addx 8\nnoop\naddx -1\naddx 2\naddx 1\nnoop\naddx 17\naddx -9\naddx 1\naddx 1\naddx -3
addx 11\nnoop\nnoop\naddx 1\nnoop\naddx 1\nnoop\nnoop\naddx -13\naddx -19\naddx 1
addx 3\naddx 26\naddx -30\naddx 12\naddx -1\naddx 3\naddx 1\nnoop\nnoop\nnoop\naddx -9
addx 18\naddx 1\naddx 2\nnoop\nnoop\naddx 9\nnoop\nnoop\nnoop\naddx -1\naddx 2
addx -37\naddx 1\naddx 3\nnoop\naddx 15\naddx -21\naddx 22\naddx -6\naddx 1\nnoop\naddx 2
addx 1\nnoop\naddx -10\nnoop\nnoop\naddx 20\naddx 1\naddx 2\naddx 2\naddx -6\naddx -11
noop\nnoop\nnoop";
    const DRAWING: &str = "
##..##..##..##..##..##..##..##..##..##..
###...###...###...###...###...###...###.
####....####....####....####....####....
#####.....#####.....#####.....#####.....
######......######......######......####
#######.......#######.......#######.....";
    #[test]
    fn test_part1() {
        assert_eq!(run(&parse(EXAMPLE)), 13140);
    }

    #[test]
    fn test_part2() {
        assert_eq!(draw(&parse(EXAMPLE)), DRAWING);
    }
}
