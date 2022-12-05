use std::collections::VecDeque;
use std::io::{self, Error, Read, Write};

fn box_msg(stacks: &mut Vec<VecDeque<char>>, moves: &Vec<Vec<usize>>, ordered: bool) -> String {
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

fn construct_stacks(blueprint: &str, size: usize) -> Vec<VecDeque<char>> {
    let mut stacks: Vec<VecDeque<char>> = vec![VecDeque::new(); size];
    for (i, c) in blueprint.chars().skip(1).step_by(4).enumerate() {
        if c != ' ' {
            stacks[i % size].push_back(c);
        }
    }
    return stacks;
}

fn main() -> Result<(), Error> {
    let mut input: String = String::new();
    io::stdin().read_to_string(&mut input).unwrap();

    let ((blueprint, indexes), moves): ((&str, &str), Vec<Vec<usize>>) = input
        .split_once("\n\n")
        .map(|(s1, s2)| {
            return (
                s1.rsplit_once("\n").unwrap(),
                s2.trim()
                    .split("\n")
                    .map(|instr| {
                        return instr
                            .split_whitespace()
                            .filter(|word| word.chars().next().unwrap().is_numeric())
                            .map(|num| num.parse::<usize>().unwrap() - 1)
                            .collect();
                    })
                    .collect(),
            );
        })
        .unwrap();
    let size = indexes.split_whitespace().last().unwrap().parse().unwrap();
    let mut stacks: Vec<VecDeque<char>>;

    stacks = construct_stacks(blueprint, size);
    writeln!(io::stdout(), "p1: {}", box_msg(&mut stacks, &moves, false)).unwrap();
    stacks = construct_stacks(blueprint, size);
    writeln!(io::stdout(), "p2: {}", box_msg(&mut stacks, &moves, true)).unwrap();
    Ok(())
}
