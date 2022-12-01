use std::io::{self, Read, Write};

fn highest_calorie_count(elves: &Vec<Vec<u32>>) -> u32 {
    let mut highest: u32 = 0;
    for line in elves {
        let mut elf_calories = 0;
        for c in line {
            elf_calories += c;
        }
        if elf_calories > highest {
            highest = elf_calories;
        }
    }
    return highest;
}

fn highest_3_calorie_count(elves: &Vec<Vec<u32>>) -> u32 {
    let mut highest_3: Vec<u32>;
    highest_3 = elves.into_iter().map(|e| e.into_iter().sum()).collect();
    highest_3.sort_by(|lhs, rhs| rhs.cmp(lhs));
    return highest_3[0] + highest_3[1] + highest_3[2];
}

fn main() -> Result<(), io::Error> {
    let mut input: String = String::new();
    io::stdin().read_to_string(&mut input)?;

    let elves: Vec<Vec<u32>>;
    elves = input
        .split("\n\n")
        .map(|l| l.split("\n").filter_map(|c| c.parse().ok()).collect())
        .collect();

    writeln!(io::stdout(), "p1: {}", highest_calorie_count(&elves))?;
    writeln!(io::stdout(), "p2: {}", highest_3_calorie_count(&elves))?;
    Ok(())
}
