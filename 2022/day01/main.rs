use std::io::{self, Error, Read, Write};

fn highest_kcal_count(elves: &Vec<Vec<u32>>, count: usize) -> u32 {
    let mut highest: Vec<u32> = elves.iter().map(|elf| elf.iter().sum()).collect();
    highest.sort();
    return highest.iter().rev().take(count).sum();
}

fn main() -> Result<(), Error> {
    let mut input: String = String::new();
    io::stdin().read_to_string(&mut input).unwrap();

    let elves: Vec<Vec<u32>> = input
        .split("\n\n")
        .map(|elf| elf.split("\n").flat_map(|kcal| kcal.parse().ok()).collect())
        .collect();

    writeln!(io::stdout(), "p1: {}", highest_kcal_count(&elves, 1)).unwrap();
    writeln!(io::stdout(), "p2: {}", highest_kcal_count(&elves, 3)).unwrap();
    return Ok(());
}
