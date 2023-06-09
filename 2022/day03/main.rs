use std::collections::HashSet;
use std::io::{self, Error, Read, Write};
use std::iter::FromIterator;

fn char_score(ch: char) -> u32 {
    if ch.is_ascii_uppercase() {
        return 27 + (ch as u32 - 'A' as u32);
    } else {
        return 01 + (ch as u32 - 'a' as u32);
    }
}

fn item_priority_sum(rucksacks: &Vec<&str>) -> u32 {
    return rucksacks
        .iter()
        .map(|ruck| ruck.split_at(ruck.len() / 2))
        .map(|(left, right)| {
            return left
                .chars()
                .find(|&ch| right.contains(ch))
                .map(char_score)
                .unwrap();
        })
        .sum();
}

fn badge_priority_sum(rucksacks: &Vec<&str>) -> u32 {
    return rucksacks
        .chunks(3)
        .map(|group_sacks| {
            return group_sacks
                .iter()
                .fold(HashSet::new(), |acc, chset| {
                    if acc.is_empty() {
                        return HashSet::from_iter(chset.chars());
                    } else {
                        return (&acc) & (&HashSet::from_iter(chset.chars()));
                    }
                })
                .into_iter()
                .next()
                .map(char_score)
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
