use std::collections::HashSet;
use std::io::{self, Error, Read, Write};
use std::iter::FromIterator;

fn item_priority_sum(rucksacks: &Vec<&str>) -> u32 {
    return rucksacks
        .iter()
        .map(|r| r.split_at(r.len() / 2))
        .map(|(c1, c2)| {
            return c1
                .chars()
                .find(|&c| c2.contains(c))
                .map(|c| {
                    if char::is_ascii_uppercase(&c) {
                        return 27 + (c as u32 - 'A' as u32);
                    } else {
                        return 01 + (c as u32 - 'a' as u32);
                    }
                })
                .unwrap();
        })
        .sum();
}

fn badge_priority_sum(rucksacks: &Vec<&str>) -> u32 {
    return rucksacks
        .chunks(3)
        .map(|group_sacks| {
            let group_item_sets: Vec<HashSet<char>> = group_sacks
                .iter()
                .map(|r| {
                    return HashSet::from_iter(r.chars());
                })
                .collect();
            return group_item_sets
                .iter()
                .skip(1)
                .fold(group_item_sets[0].clone(), |cur, next| (&cur) & (&next))
                .into_iter()
                .next()
                .map(|c| {
                    if char::is_ascii_uppercase(&c) {
                        return 27 + (c as u32 - 'A' as u32);
                    } else {
                        return 01 + (c as u32 - 'a' as u32);
                    }
                })
                .unwrap();
        })
        .sum();
}

fn main() -> Result<(), Error> {
    let mut input: String = String::new();
    io::stdin().read_to_string(&mut input)?;
    let rucksacks: Vec<&str> = parse(&input);
    writeln!(io::stdout(), "p1: {}", item_priority_sum(&rucksacks))?;
    writeln!(io::stdout(), "p2: {}", badge_priority_sum(&rucksacks))?;
    Ok(())
}

fn parse(input: &str) -> Vec<&str> {
    return input.trim().split("\n").collect();
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE: &str = "\
vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw";
    #[test]
    fn test_part1() {
        assert_eq!(item_priority_sum(&parse(EXAMPLE)), 157);
    }

    #[test]
    fn test_part2() {
        assert_eq!(badge_priority_sum(&parse(EXAMPLE)), 70);
    }
}
