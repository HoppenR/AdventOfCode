use std::collections::HashSet;
use std::io::{self, Error, Read, Write};

fn item_priority_sum(rucksacks: &Vec<&str>) -> u32 {
    return rucksacks
        .into_iter()
        .map(|r| r.split_at(r.len() / 2))
        .map(|(c1, c2)| {
            for c1ch in c1.chars() {
                for c2ch in c2.chars() {
                    if c1ch == c2ch {
                        if char::is_ascii_uppercase(&c1ch) {
                            return 27 + (c1ch as u32 - 'A' as u32);
                        } else {
                            return 01 + (c1ch as u32 - 'a' as u32);
                        }
                    }
                }
            }
            return 0;
        })
        .sum();
}

fn badge_priority_sum(rucksacks: &Vec<&str>) -> u32 {
    let mut score: u32 = 0;
    for group_sacks in rucksacks.chunks(3) {
        let mut items: HashSet<char> = HashSet::new();
        for r in group_sacks {
            let mut backpack: HashSet<char> = HashSet::new();
            for c in r.chars() {
                backpack.insert(c);
            }
            if items.len() == 0 {
                items = backpack;
            } else {
                items = items.intersection(&backpack).cloned().collect();
            }
        }
        for c in items {
            if char::is_ascii_uppercase(&c) {
                score += 27 + (c as u32 - 'A' as u32);
            } else {
                score += 01 + (c as u32 - 'a' as u32);
            }
        }
    }
    return score;
}

fn main() -> Result<(), Error> {
    let mut input: String = String::new();
    io::stdin().read_to_string(&mut input).unwrap();

    let rucksacks: Vec<&str> = input.trim().split("\n").collect();

    writeln!(io::stdout(), "p1: {}", item_priority_sum(&rucksacks)).unwrap();
    writeln!(io::stdout(), "p2: {}", badge_priority_sum(&rucksacks)).unwrap();
    Ok(())
}
