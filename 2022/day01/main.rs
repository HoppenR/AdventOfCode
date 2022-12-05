use std::io::{self, Error, Read, Write};

fn highest_kcal_count(elves: &Vec<Vec<u32>>, count: usize) -> u32 {
    let mut highest: Vec<u32> = elves.iter().map(|elf| elf.iter().sum()).collect();
    highest.sort();
    return highest.iter().rev().take(count).sum();
}

fn main() -> Result<(), Error> {
    let mut input: String = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let elves: Vec<Vec<u32>> = parse_elves(&input);

    writeln!(io::stdout(), "p1: {}", highest_kcal_count(&elves, 1)).unwrap();
    writeln!(io::stdout(), "p2: {}", highest_kcal_count(&elves, 3)).unwrap();
    return Ok(());
}

fn parse_elves(input: &String) -> Vec<Vec<u32>> {
    return input
        .split("\n\n")
        .map(|elf| elf.split("\n").flat_map(|kcal| kcal.parse().ok()).collect())
        .collect();
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000";
    #[test]
    fn test_part1() {
        let elves = parse_elves(&String::from(INPUT));
        assert_eq!(highest_kcal_count(&elves, 1), 24000);
    }

    #[test]
    fn test_part2() {
        let elves = parse_elves(&String::from(INPUT));
        assert_eq!(highest_kcal_count(&elves, 3), 45000);
    }
}
