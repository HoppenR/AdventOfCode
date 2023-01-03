use std::collections::VecDeque;
use std::io::{self, Error, Read, Write};

#[derive(Clone)]
enum Operation {
    Add(u64),
    Mul(u64),
    Sqr,
}

#[derive(Clone)]
struct Monkey {
    items: VecDeque<u64>,
    op: Operation,
    div_condition: u64,
    true_target: usize,
    false_target: usize,
    inspect_amount: usize,
}

fn monkey_lvl(monkey_init_state: &Vec<Monkey>, rounds: u32, div3: bool) -> usize {
    let mut monkeys = monkey_init_state.clone();
    let lcm_div_conditions = monkeys
        .iter()
        .map(|m| m.div_condition)
        .fold(1, |acc, val| acc * val);
    for _ in 0..rounds {
        for m_idx in 0..monkeys.len() {
            while monkeys[m_idx].items.len() > 0 {
                let mut item = monkeys[m_idx].items.pop_front().unwrap();
                monkeys[m_idx].inspect_amount += 1;
                match monkeys[m_idx].op {
                    Operation::Add(amount) => item += amount,
                    Operation::Mul(amount) => item *= amount,
                    Operation::Sqr => item *= item,
                }
                if div3 {
                    item /= 3;
                } else {
                    item %= lcm_div_conditions;
                }
                let target: usize = match item % monkeys[m_idx].div_condition {
                    0 => monkeys[m_idx].true_target,
                    _ => monkeys[m_idx].false_target,
                };
                monkeys[target].items.push_back(item);
            }
        }
    }
    let mut total_inspects = monkeys
        .iter()
        .map(|m| m.inspect_amount)
        .collect::<Vec<usize>>();
    total_inspects.sort_by(|lhs, rhs| usize::cmp(rhs, lhs));
    return total_inspects[0] * total_inspects[1];
}

fn main() -> Result<(), Error> {
    let mut input: String = String::new();
    io::stdin().read_to_string(&mut input)?;
    let monkeys: Vec<Monkey> = parse(&input);
    writeln!(io::stdout(), "p1: {}", monkey_lvl(&monkeys, 20, true))?;
    writeln!(io::stdout(), "p2: {}", monkey_lvl(&monkeys, 10000, false))?;
    Ok(())
}

fn parse(input: &str) -> Vec<Monkey> {
    return input
        .trim()
        .split("\n\n")
        .map(|monkey_str| {
            let monkey_lines: Vec<&str> = monkey_str.split("\n").skip(1).collect();
            return Monkey {
                items: monkey_lines[0]
                    .split_once(": ")
                    .map(|(_, itm_str)| {
                        return itm_str.split(", ").map(|i| i.parse().unwrap()).collect();
                    })
                    .unwrap(),
                op: monkey_lines[1]
                    .split("old ")
                    .last()
                    .unwrap()
                    .split_once(" ")
                    .map(|(t_str, amt_str)| match t_str {
                        "+" => {
                            return Operation::Add(amt_str.parse().unwrap());
                        }
                        "*" => match amt_str {
                            "old" => return Operation::Sqr,
                            _ => return Operation::Mul(amt_str.parse().unwrap()),
                        },
                        _ => unreachable!(),
                    })
                    .unwrap(),
                div_condition: monkey_lines[2]
                    .rsplit_once(" ")
                    .map(|(_, cond_str)| cond_str.parse().unwrap())
                    .unwrap(),
                true_target: monkey_lines[3]
                    .rsplit_once(" ")
                    .map(|(_, target_str)| target_str.parse().unwrap())
                    .unwrap(),
                false_target: monkey_lines[4]
                    .rsplit_once(" ")
                    .map(|(_, target_str)| target_str.parse().unwrap())
                    .unwrap(),
                inspect_amount: 0,
            };
        })
        .collect();
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE: &str = "\
Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1";
    #[test]
    fn test_part1() {
        assert_eq!(monkey_lvl(&parse(EXAMPLE), 20, true), 10605);
    }

    #[test]
    fn test_part2() {
        assert_eq!(monkey_lvl(&parse(EXAMPLE), 10000, false), 2713310158);
    }
}
