use std::collections::VecDeque;
use std::io::{self, Error, Read, Write};

fn box_msg(oldstacks: &Vec<VecDeque<char>>, moves: &Vec<Vec<usize>>, ordered: bool) -> String {
    let mut stacks = oldstacks.clone();
    for mov in moves {
        let (amount, source, target) = (mov[0], mov[1], mov[2]);
        let mut crane: VecDeque<char> = VecDeque::new();
        for _ in 0..=amount {
            if ordered {
                crane.push_front(stacks[source].pop_front().unwrap());
            } else {
                crane.push_back(stacks[source].pop_front().unwrap());
            }
        }
        for _ in 0..=amount {
            stacks[target].push_front(crane.pop_front().unwrap())
        }
    }
    return stacks.iter().map(|s| *s.front().unwrap()).collect();
}

fn main() -> Result<(), Error> {
    let mut input: String = String::new();
    io::stdin().read_to_string(&mut input)?;
    let (stacks, moves): (Vec<VecDeque<char>>, Vec<Vec<usize>>) = parse(&input);
    writeln!(io::stdout(), "p1: {}", box_msg(&stacks, &moves, false))?;
    writeln!(io::stdout(), "p2: {}", box_msg(&stacks, &moves, true))?;
    Ok(())
}

fn parse(input: &str) -> (Vec<VecDeque<char>>, Vec<Vec<usize>>) {
    return input
        .split_once("\n\n")
        .map(|(s1, s2)| {
            return (
                s1.rsplit_once("\n")
                    .map(|(blueprint, indexes)| {
                        let size = indexes.split_whitespace().last().unwrap().parse().unwrap();
                        let mut stacks: Vec<VecDeque<char>> = vec![VecDeque::new(); size];
                        blueprint
                            .chars()
                            .skip(1)
                            .step_by(4)
                            .enumerate()
                            .filter(|(_, c)| *c != ' ')
                            .for_each(|(i, c)| stacks[i % size].push_back(c));
                        return stacks;
                    })
                    .unwrap(),
                s2.trim()
                    .split("\n")
                    .map(|instr| {
                        return instr
                            .split_whitespace()
                            .skip(1)
                            .step_by(2)
                            .map(|num| num.parse::<usize>().unwrap() - 1)
                            .collect();
                    })
                    .collect(),
            );
        })
        .unwrap();
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE: &str = "    [D]    \n[N] [C]    \n[Z] [M] [P]\n1   2   3 \n
move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2";
    #[test]
    fn test_part1() {
        let (stacks, moves): (Vec<VecDeque<char>>, Vec<Vec<usize>>) = parse(EXAMPLE);
        assert_eq!(box_msg(&stacks, &moves, false), "CMZ");
    }

    #[test]
    fn test_part2() {
        let (stacks, moves): (Vec<VecDeque<char>>, Vec<Vec<usize>>) = parse(EXAMPLE);
        assert_eq!(box_msg(&stacks, &moves, true), "MCD");
    }
}
