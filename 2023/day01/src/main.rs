use std::io::{self, Error, Read, Write};

const WRITTEN_NUMS: [(&str, u32); 9] = [
    ("one", 1),
    ("two", 2),
    ("three", 3),
    ("four", 4),
    ("five", 5),
    ("six", 6),
    ("seven", 7),
    ("eight", 8),
    ("nine", 9),
];

enum CalDig {
    Literal(u32),
    Spelled(u32),
}

fn filter_digits(x: &CalDig, extnum: bool) -> Option<&u32> {
    match (x, extnum) {
        (CalDig::Literal(v), _) => Some(v),
        (CalDig::Spelled(v), true) => Some(v),
        _ => None,
    }
}

fn sum_calibration(digit_lines: &Vec<Vec<CalDig>>, extnum: bool) -> u32 {
    return digit_lines
        .iter()
        .map(|line| {
            line.iter()
                .filter_map(|x| filter_digits(x, extnum))
                .collect::<Vec<_>>()
        })
        .map(|digits| {
            format!("{}{}", digits.first().unwrap(), digits.last().unwrap())
                .parse::<u32>()
                .unwrap()
        })
        .sum();
}

fn main() -> Result<(), Error> {
    let mut input: String = String::new();
    io::stdin().read_to_string(&mut input)?;
    let lines: Vec<Vec<CalDig>> = parse(&input.trim());
    writeln!(io::stdout(), "p1: {}", sum_calibration(&lines, false))?;
    writeln!(io::stdout(), "p2: {}", sum_calibration(&lines, true))?;
    Ok(())
}

fn parse(input: &str) -> Vec<Vec<CalDig>> {
    input
        .lines()
        .map(|line| {
            line.chars()
                .enumerate()
                .filter_map(|(offset, cur)| {
                    if cur.is_digit(10) {
                        Some(CalDig::Literal(cur.to_digit(10).unwrap()))
                    } else {
                        WRITTEN_NUMS
                            .iter()
                            .find(|(k, _)| line[offset..].starts_with(*k))
                            .map(|(_, v)| CalDig::Spelled(*v))
                    }
                })
                .collect()
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE1: &str = "\
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet";

    const EXAMPLE2: &str = "\
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen";

    #[test]
    fn test_part1() {
        assert_eq!(sum_calibration(&parse(EXAMPLE1), false), 142);
    }

    #[test]
    fn test_part2() {
        assert_eq!(sum_calibration(&parse(EXAMPLE2), true), 281);
    }
}
