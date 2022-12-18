use std::cmp::{max, min};
use std::collections::HashSet;
use std::io::{self, Error, Read, Write};

#[derive(Hash, Eq, PartialEq, Clone, Copy)]
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
        map.insert(*sand);
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
        if map.insert(*sand) {
            return 1;
        } else {
            return -1;
        };
    }
    *sand = check;
    return 0;
}

fn drop_sand(old_map: &HashSet<Point>, floor: bool) -> usize {
    let drop_point = Point { x: 500, y: 0 };
    let mut map: HashSet<Point> = old_map.clone();

    let maxy = map.iter().max_by(|lhs, rhs| lhs.y.cmp(&rhs.y)).unwrap().y;

    let mut sand = drop_point;
    loop {
        match iterate_sand(&mut map, &mut sand, maxy, floor) {
            0 => continue,
            1 => sand = drop_point,
            -1 => break,
            _ => unreachable!(),
        }
    }
    return map.len() - old_map.len();
}

fn main() -> Result<(), Error> {
    let mut input: String = String::new();
    io::stdin().read_to_string(&mut input).unwrap();

    let instructions: Vec<Vec<Point>> = input
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

    let map: HashSet<Point> = construct_map(&instructions);

    writeln!(io::stdout(), "p1: {}", drop_sand(&map, false)).unwrap();
    writeln!(io::stdout(), "p2: {}", drop_sand(&map, true)).unwrap();
    Ok(())
}
