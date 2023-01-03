use std::cmp::{max, min};
use std::collections::HashSet;
use std::io::{self, Error, Read, Write};

#[derive(Hash, Eq, PartialEq, Clone)]
struct Point {
    x: u32,
    y: u32,
}

fn construct_map(input: &Vec<Vec<Point>>) -> HashSet<Point> {
    let mut map: HashSet<Point> = HashSet::new();
    for instructions in input {
        for instr in instructions.windows(2) {
            let xdiff: bool = instr[0].x != instr[1].x;
            if xdiff {
                for x in min(instr[0].x, instr[1].x)..=max(instr[0].x, instr[1].x) {
                    map.insert(Point { x, y: instr[0].y });
                }
            } else {
                for y in min(instr[0].y, instr[1].y)..=max(instr[0].y, instr[1].y) {
                    map.insert(Point { x: instr[0].x, y });
                }
            }
        }
    }
    return map;
}

// -1 = abort
//  0 = continue
//  1 = added
fn iterate_sand(map: &mut HashSet<Point>, sand: &mut Point, maxy: u32, floor: bool) -> i32 {
    if floor && sand.y == maxy + 1 {
        map.insert(sand.clone());
        return 1;
    } else if !floor && sand.y == maxy {
        return -1;
    }
    let mut check = Point {
        x: sand.x,
        y: sand.y + 1,
    };
    if map.contains(&check) {
        check.x -= 1;
        if !(map.contains(&check)) {
            *sand = check;
            return 0;
        }
        check.x += 2;
        if !(map.contains(&check)) {
            *sand = check;
            return 0;
        }
        if map.insert(sand.clone()) {
            return 1;
        } else {
            return -1;
        };
    }
    *sand = check;
    return 0;
}

fn drop_sand(instructions: &Vec<Vec<Point>>, floor: bool) -> usize {
    let mut map = construct_map(instructions);
    let stone_count = map.len();
    let maxy = map.iter().max_by(|lhs, rhs| lhs.y.cmp(&rhs.y)).unwrap().y;

    let mut sand = Point { x: 500, y: 0 };
    loop {
        match iterate_sand(&mut map, &mut sand, maxy, floor) {
            0 => continue,
            1 => sand = Point { x: 500, y: 0 },
            -1 => break,
            _ => unreachable!(),
        }
    }
    return map.len() - stone_count;
}

fn main() -> Result<(), Error> {
    let mut input: String = String::new();
    io::stdin().read_to_string(&mut input)?;
    let map: Vec<Vec<Point>> = parse(&input);
    writeln!(io::stdout(), "p1: {}", drop_sand(&map, false))?;
    writeln!(io::stdout(), "p2: {}", drop_sand(&map, true))?;
    Ok(())
}

fn parse(input: &str) -> Vec<Vec<Point>> {
    return input
        .lines()
        .map(|l| {
            l.split(" -> ")
                .map(|instr_str| {
                    let (xstr, ystr) = instr_str.split_once(",").unwrap();
                    return Point {
                        x: xstr.parse().unwrap(),
                        y: ystr.parse().unwrap(),
                    };
                })
                .collect()
        })
        .collect();
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE: &str = "\
498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9";
    #[test]
    fn test_part1() {
        assert_eq!(drop_sand(&parse(EXAMPLE), false), 24);
    }

    #[test]
    fn test_part2() {
        assert_eq!(drop_sand(&parse(EXAMPLE), true), 93);
    }
}
