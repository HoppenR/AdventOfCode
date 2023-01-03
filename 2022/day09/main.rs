use std::collections::HashSet;
use std::io::{self, Error, Read, Write};

fn manhattan_distance(head: (i32, i32), tail: &(i32, i32)) -> i32 {
    return i32::abs(head.0 - tail.0) + i32::abs(head.1 - tail.1);
}

fn move_tail_toward_head(head: (i32, i32), tail: &mut (i32, i32), xwise: bool, ywise: bool) {
    if xwise {
        if tail.0 < head.0 {
            tail.0 += 1;
        } else {
            tail.0 -= 1;
        }
    }
    if ywise {
        if tail.1 < head.1 {
            tail.1 += 1;
        } else {
            tail.1 -= 1;
        }
    }
}

fn next_tail_iteration(head: (i32, i32), tail: &mut (i32, i32)) {
    match manhattan_distance(head, tail) {
        2 => {
            if i32::abs(head.0 - tail.0) != i32::abs(head.1 - tail.1) {
                if head.0 != tail.0 {
                    move_tail_toward_head(head, tail, true, false);
                } else {
                    move_tail_toward_head(head, tail, false, true);
                }
            }
        }
        3 | 4 => {
            move_tail_toward_head(head, tail, true, true);
        }
        _ => {}
    }
}

fn n_unique_nth_tail_pos(moves: &Vec<(char, i32)>, nth_tail: usize) -> usize {
    let mut seen: HashSet<(i32, i32)> = HashSet::new();
    let mut tail_positions: Vec<(i32, i32)> = Vec::new();
    tail_positions.resize(nth_tail + 1, (0, 0));
    for (direction, amount) in moves {
        for _ in 0..*amount {
            match direction {
                'U' => tail_positions[0].1 += 1,
                'L' => tail_positions[0].0 -= 1,
                'D' => tail_positions[0].1 -= 1,
                'R' => tail_positions[0].0 += 1,
                _ => {}
            }
            for tail_ix in 1..=nth_tail {
                let local_head: (i32, i32) = tail_positions[tail_ix - 1];
                let mut local_tail: &mut (i32, i32) = tail_positions.get_mut(tail_ix).unwrap();
                next_tail_iteration(local_head, &mut local_tail);
            }
            seen.insert(tail_positions[nth_tail]);
        }
    }
    return seen.len();
}

fn main() -> Result<(), Error> {
    let mut input: String = String::new();
    io::stdin().read_to_string(&mut input)?;
    let moves: Vec<(char, i32)> = parse(&input);
    writeln!(io::stdout(), "p1: {}", n_unique_nth_tail_pos(&moves, 1))?;
    writeln!(io::stdout(), "p2: {}", n_unique_nth_tail_pos(&moves, 9))?;
    Ok(())
}

fn parse(input: &str) -> Vec<(char, i32)> {
    return input
        .trim()
        .split("\n")
        .map(|l| {
            return l
                .split_once(" ")
                .map(|(left, right)| {
                    return (left.chars().next().unwrap(), right.parse().unwrap());
                })
                .unwrap();
        })
        .collect();
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLES: [&str; 2] = [
        "\
R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2",
        "\
R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20",
    ];
    #[test]
    fn test_part1() {
        assert_eq!(n_unique_nth_tail_pos(&parse(EXAMPLES[0]), 1), 13);
    }

    #[test]
    fn test_part2() {
        assert_eq!(n_unique_nth_tail_pos(&parse(EXAMPLES[0]), 9), 1);
        assert_eq!(n_unique_nth_tail_pos(&parse(EXAMPLES[1]), 9), 36);
    }
}
