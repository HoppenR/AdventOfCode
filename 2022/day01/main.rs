use std::cmp::Reverse;
use std::io::{self, Error, Read, Write};

fn highest_kcal_count(old_elves: &Vec<u32>, count: usize) -> u32 {
    let mut highest = old_elves.clone();
    highest.sort_by_key(|&num| Reverse(num));
    return highest.iter().take(count).sum();
}

fn main() -> Result<(), Error> {
    let mut input: String = String::new();
    io::stdin().read_to_string(&mut input)?;
    let elves: Vec<u32> = parse(&input);
    writeln!(io::stdout(), "p1: {}", highest_kcal_count(&elves, 1))?;
    writeln!(io::stdout(), "p2: {}", highest_kcal_count(&elves, 3))?;
    Ok(())
}

fn parse(input: &str) -> Vec<u32> {
    return input
        .split("\n\n")
        .map(|elf| {
            return elf
                .split("\n")
                .map(|kcal| kcal.parse::<u32>().unwrap())
                .sum();
        })
        .collect();
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE: &str = "\
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000";
    #[test]
    fn test_part1() {
        assert_eq!(highest_kcal_count(&parse(EXAMPLE), 1), 24000);
    }

    #[test]
    fn test_part2() {
        assert_eq!(highest_kcal_count(&parse(EXAMPLE), 3), 45000);
    }
}
