use std::io::{self, Error, Read, Write};
use std::str::FromStr;

#[derive(Debug)]
struct RangeError;

struct Range {
    start: u32,
    end: u32,
}

impl FromStr for Range {
    type Err = RangeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (left, right) = s
            .split_once("-")
            .map(|(start, end)| (start.parse().unwrap(), end.parse().unwrap()))
            .unwrap();

        return Ok(Range {
            start: left,
            end: right,
        });
    }
}

fn full_overlap(left: &Range, right: &Range) -> bool {
    left.start >= right.start && left.end <= right.end
}

fn part_overlap(left: &Range, right: &Range) -> bool {
    left.start <= right.end && left.end >= right.start
}

fn overlap_cnt(
    assignments: &Vec<(Range, Range)>,
    comp_func: &dyn Fn(&Range, &Range) -> bool,
) -> u32 {
    return assignments
        .iter()
        .filter(|(left, right)| comp_func(left, right) || comp_func(right, left))
        .count() as u32;
}

fn main() -> Result<(), Error> {
    let mut input: String = String::new();
    io::stdin().read_to_string(&mut input).unwrap();

    let pairs: Vec<(Range, Range)> = input
        .trim()
        .lines()
        .map(|l| {
            l.split_once(",")
                .map(|(l, r)| (l.parse().unwrap(), r.parse().unwrap()))
                .unwrap()
        })
        .collect();

    writeln!(io::stdout(), "p1: {}", overlap_cnt(&pairs, &full_overlap)).unwrap();
    writeln!(io::stdout(), "p2: {}", overlap_cnt(&pairs, &part_overlap)).unwrap();
    Ok(())
}
