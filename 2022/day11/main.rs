use std::{
    collections::VecDeque,
    io::{self, Error, Read, Write},
};

#[derive(Clone)]
enum OpType {
    Add,
    Mul,
    Sqr,
}

#[derive(Clone)]
struct Monkey {
    items: VecDeque<u128>,
    op_type: OpType,
    op_amount: u128,
    div_condition: u128,
    true_target: usize,
    false_target: usize,
    inspect_amount: usize,
}

fn calc_monkey_business(monkey_init_state: &Vec<Monkey>) -> usize {
    let mut monkeys = monkey_init_state.clone();
    for _round in 1..=20 {
        println!("ROUND: {}", _round);
        for m_idx in 0..monkeys.len() {
            println!("Monkey {}:", m_idx);
            while monkeys[m_idx].items.len() > 0 {
                let mut item = monkeys[m_idx].items.pop_front().unwrap();
                monkeys[m_idx].inspect_amount += 1;
                println!("\tMonkey inspects an item with worry level {}.", item);
                match monkeys[m_idx].op_type {
                    OpType::Add => {
                        item += monkeys[m_idx].op_amount;
                        println!(
                            "\t\tWorry level increases by {} to {}.",
                            monkeys[m_idx].op_amount, item
                        );
                    }
                    OpType::Mul => {
                        item *= monkeys[m_idx].op_amount;
                        println!(
                            "\t\tWorry level is mulitiplied by {} to {}.",
                            monkeys[m_idx].op_amount, item
                        );
                    }
                    OpType::Sqr => {
                        item *= item;
                        println!("\t\tWorry level is multiplied by itself to {}.", item);
                    }
                }
                item /= 3;
                println!(
                    "\t\tMonkey gets bored with item. Worry level is divided by 3 to {}.",
                    item
                );
                if item % monkeys[m_idx].div_condition == 0 {
                    let target = monkeys[m_idx].true_target;
                    println!(
                        "\t\tCurrent worry level is divisible by {}.",
                        monkeys[m_idx].div_condition
                    );
                    monkeys[target].items.push_back(item);
                    println!("\t\tItem with worry level {} is thrown to {}", item, target);
                } else {
                    println!(
                        "\t\tCurrent worry level is not divisible by {}.",
                        monkeys[m_idx].div_condition
                    );
                    let target = monkeys[m_idx].false_target;
                    println!("\t\tItem with worry level {} is thrown to {}", item, target);
                    monkeys[target].items.push_back(item);
                }
            }
        }
    }
    let mut inspect_amounts = monkeys
        .iter()
        .map(|m| m.inspect_amount)
        .collect::<Vec<usize>>();
    inspect_amounts.sort_by(|f, l| usize::cmp(l, f));
    let monkey_business = inspect_amounts.into_iter().take(2).fold(1, |i, w| i * w);
    return monkey_business;
}

fn main() -> Result<(), Error> {
    let mut input: String = String::new();
    io::stdin().read_to_string(&mut input).unwrap();

    let monkeys: Vec<Monkey> = input
        .trim()
        .split("\n\n")
        .map(|monkey_str| {
            let monkey_lines: Vec<&str> = monkey_str.split("\n").skip(1).collect();

            let items: VecDeque<u128> = monkey_lines[0]
                .split_once(": ")
                .map(|(_, itm_str)| {
                    return itm_str.split(", ").map(|i| i.parse().unwrap()).collect();
                })
                .unwrap();

            let (_, op_type_str) = monkey_lines[1].split_once("old ").unwrap();
            let (op_type, op_amount): (OpType, u128) = op_type_str
                .split_once(" ")
                .map(|(t_str, amt_str)| match t_str {
                    "+" => {
                        return (OpType::Add, amt_str.parse().unwrap());
                    }
                    "*" => match amt_str {
                        "old" => {
                            return (OpType::Sqr, 0);
                        }
                        _ => {
                            return (OpType::Mul, amt_str.parse().unwrap());
                        }
                    },
                    _ => unreachable!(),
                })
                .unwrap();

            let div_condition: u128 = monkey_lines[2]
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

    writeln!(io::stdout(), "p1: {}", calc_monkey_business(&monkeys)).unwrap();
    Ok(())
}
