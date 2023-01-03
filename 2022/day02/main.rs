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
    io::stdin().read_to_string(&mut input)?;
    let rounds: Vec<Vec<usize>> = parse(&input);
    writeln!(io::stdout(), "p1: {}", count_score(&rounds, 1))?;
    writeln!(io::stdout(), "p2: {}", count_score(&rounds, 2))?;
    Ok(())
}

fn parse(input: &str) -> Vec<Vec<usize>> {
    return input
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
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE: &str = "A Y\nB X\nC Z";
    #[test]
    fn test_part1() {
        assert_eq!(count_score(&parse(EXAMPLE), 1), 15);
    }

    #[test]
    fn test_part2() {
        assert_eq!(count_score(&parse(EXAMPLE), 2), 12);
    }
}
