use std::collections::VecDeque;
use std::io::{self, Error, Read, Write};

#[derive(Clone)]
enum OpType {
    Add,
    Mul,
    Sqr,
}

#[derive(Clone)]
struct Monkey {
    items: VecDeque<u64>,
    op_type: OpType,
    op_amount: u64,
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
                match monkeys[m_idx].op_type {
                    OpType::Add => item += monkeys[m_idx].op_amount,
                    OpType::Mul => item *= monkeys[m_idx].op_amount,
                    OpType::Sqr => item *= item,
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
    let mut inspect_amounts = monkeys
        .iter()
        .map(|m| m.inspect_amount)
        .collect::<Vec<usize>>();
    inspect_amounts.sort_by(|first, last| usize::cmp(last, first));
    return inspect_amounts[0] * inspect_amounts[1];
}

fn main() -> Result<(), Error> {
    let mut input: String = String::new();
    io::stdin().read_to_string(&mut input).unwrap();

    let monkeys: Vec<Monkey> = input
        .trim()
        .split("\n\n")
        .map(|monkey_str| {
            let monkey_lines: Vec<&str> = monkey_str.split("\n").skip(1).collect();
            let items: VecDeque<u64> = monkey_lines[0]
                .split_once(": ")
                .map(|(_, itm_str)| {
                    return itm_str.split(", ").map(|i| i.parse().unwrap()).collect();
                })
                .unwrap();
            let (_, op_type_str) = monkey_lines[1].split_once("old ").unwrap();
            let (op_type, op_amount): (OpType, u64) = op_type_str
                .split_once(" ")
                .map(|(t_str, amt_str)| match t_str {
                    "+" => {
                        return (OpType::Add, amt_str.parse().unwrap());
                    }
                    "*" => match amt_str {
                        "old" => return (OpType::Sqr, 0),
                        _ => return (OpType::Mul, amt_str.parse().unwrap()),
                    },
                    _ => unreachable!(),
                })
                .unwrap();
            let div_condition: u64 = monkey_lines[2]
                .rsplit_once(" ")
                .map(|(_, cond_str)| cond_str.parse().unwrap())
                .unwrap();
            let true_target: usize = monkey_lines[3]
                .rsplit_once(" ")
                .map(|(_, target_str)| target_str.parse().unwrap())
                .unwrap();
            let false_target: usize = monkey_lines[4]
                .rsplit_once(" ")
                .map(|(_, target_str)| target_str.parse().unwrap())
                .unwrap();
            return Monkey {
                items,
                op_type,
                op_amount,
                div_condition,
                true_target,
                false_target,
                inspect_amount: 0,
            };
        })
        .collect();

    writeln!(io::stdout(), "p1: {}", monkey_lvl(&monkeys, 20, true)).unwrap();
    writeln!(io::stdout(), "p2: {}", monkey_lvl(&monkeys, 10000, false)).unwrap();
    Ok(())
}
