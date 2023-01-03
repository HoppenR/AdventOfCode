use std::io::{self, Error, Read, Write};
use std::ops::Range;

fn full_overlap(left: &Range<u32>, right: &Range<u32>) -> bool {
    return left.start >= right.start && left.end <= right.end;
}

fn part_overlap(left: &Range<u32>, right: &Range<u32>) -> bool {
    return left.start <= right.end && left.end >= right.start;
}

fn overlap_cnt(
    assignments: &Vec<(Range<u32>, Range<u32>)>,
    comp_func: &dyn Fn(&Range<u32>, &Range<u32>) -> bool,
) -> usize {
    return assignments
        .iter()
        .filter(|(left, right)| comp_func(left, right) || comp_func(right, left))
        .count();
}

fn main() -> Result<(), Error> {
    let mut input: String = String::new();
    io::stdin().read_to_string(&mut input)?;
    let pairs: Vec<(Range<u32>, Range<u32>)> = parse(&input);
    writeln!(io::stdout(), "p1: {}", overlap_cnt(&pairs, &full_overlap))?;
    writeln!(io::stdout(), "p2: {}", overlap_cnt(&pairs, &part_overlap))?;
    Ok(())
}

fn parse(input: &str) -> Vec<(Range<u32>, Range<u32>)> {
    return input
        .trim()
        .lines()
        .map(|line| {
            return line
                .split_once(",")
                .map(|(left, right)| {
                    let (start1, end1) = left.split_once("-").unwrap();
                    let (start2, end2) = right.split_once("-").unwrap();
                    return (
                        start1.parse().unwrap()..end1.parse().unwrap(),
                        start2.parse().unwrap()..end2.parse().unwrap(),
                    );
                })
                .unwrap();
        })
        .collect();
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE: &str = "\
2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8";
    #[test]
    fn test_part1() {
        assert_eq!(overlap_cnt(&parse(EXAMPLE), &full_overlap), 2);
    }

    #[test]
    fn test_part2() {
        assert_eq!(overlap_cnt(&parse(EXAMPLE), &part_overlap), 4);
    }
}
