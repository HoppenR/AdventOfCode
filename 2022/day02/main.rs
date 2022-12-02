use std::io::{self, Error, Read, Write};

const LOSS: [usize; 4] = [0xBABECAFE, 3, 1, 2];
const DRAW: [usize; 4] = [0xBABECAFE, 1, 2, 3];
const WINS: [usize; 4] = [0xBABECAFE, 2, 3, 1];

fn count_score(rounds: &Vec<Vec<usize>>, ruleset: u8) -> usize {
    return rounds
        .iter()
        .filter_map(|rd| match ruleset {
            1 => Some(
                rd[1]
                    + 0 * (LOSS[rd[0]] == rd[1]) as usize
                    + 3 * (DRAW[rd[0]] == rd[1]) as usize
                    + 6 * (WINS[rd[0]] == rd[1]) as usize,
            ),
            2 => match rd[1] {
                1 => Some(0 + LOSS[rd[0]]),
                2 => Some(3 + DRAW[rd[0]]),
                3 => Some(6 + WINS[rd[0]]),
                _ => None,
            },
            _ => None,
        })
        .sum();
}

fn main() -> Result<(), Error> {
    let mut input: String = String::new();
    io::stdin().read_to_string(&mut input).unwrap();

    let rounds: Vec<Vec<usize>> = input
        .trim()
        .split('\n')
        .map(|l| {
            l.split(' ')
                .filter_map(|c| match c {
                    "A" | "X" => Some(1),
                    "B" | "Y" => Some(2),
                    "C" | "Z" => Some(3),
                    _ => None,
                })
                .collect()
        })
        .collect();

    writeln!(io::stdout(), "p1: {}", count_score(&rounds, 1)).unwrap();
    writeln!(io::stdout(), "p1: {}", count_score(&rounds, 2)).unwrap();
    return Ok(());
}
