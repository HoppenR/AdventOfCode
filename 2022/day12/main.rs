use std::collections::VecDeque;
use std::io::{self, Error, Read, Write};

#[derive(Hash, Eq, PartialEq, Clone)]
struct Point {
    x: i32,
    y: i32,
}

fn find(map: &Vec<Vec<char>>, ch: char) -> Point {
    for y in 0..map.len() {
        for x in 0..map.len() {
            if map[y][x] == ch {
                return Point {
                    x: x as i32,
                    y: y as i32,
                };
            }
        }
    }
    unreachable!();
}

fn find_shortest_path(map: &Vec<Vec<char>>, reverse: bool) -> usize {
    let start: Point = if reverse {
        find(map, 'E')
    } else {
        find(map, 'S')
    };
    let mut queue: VecDeque<Point> = VecDeque::new();
    let mut seen: VecDeque<Point> = VecDeque::new();
    let mut scores: VecDeque<usize> = VecDeque::new();
    let checks: Vec<Point> = vec![
        Point { y: 1, x: 0 },
        Point { y: 0, x: -1 },
        Point { y: -1, x: 0 },
        Point { y: 0, x: 1 },
    ];
    scores.push_back(0);
    queue.push_back(start);
    while queue.len() > 0 {
        let cur_point = queue.pop_front().unwrap();
        let mut cur_elevation = map[cur_point.y as usize][cur_point.x as usize];
        if cur_elevation == 'S' {
            cur_elevation = 'a';
        } else if cur_elevation == 'E' {
            cur_elevation = 'z';
        }
        let cost = scores.pop_front().unwrap();
        for check in &checks {
            let comp = Point {
                y: cur_point.y + check.y,
                x: cur_point.x + check.x,
            };
            if comp.y < 0 || comp.y >= map.len() as i32 {
                continue;
            }
            if comp.x < 0 || comp.x >= map[0].len() as i32 {
                continue;
            }
            let target_elevation = map[comp.y as usize][comp.x as usize];
            if (!reverse && cur_elevation >= 'y' && target_elevation == 'E')
                || (reverse && cur_elevation <= 'b' && target_elevation == 'a')
            {
                return cost + 1;
            }
            if (!reverse && can_move(cur_elevation, target_elevation))
                || (reverse && can_move(target_elevation, cur_elevation))
            {
                if !seen.contains(&comp) {
                    seen.push_back(comp.clone());
                    queue.push_back(comp.clone());
                    scores.push_back(cost + 1);
                }
            }
        }
    }
    unreachable!();
}

fn can_move(from: char, to: char) -> bool {
    if to <= from {
        return true;
    } else {
        return to as u32 - from as u32 == 1;
    }
}

fn main() -> Result<(), Error> {
    let mut input: String = String::new();
    io::stdin().read_to_string(&mut input).unwrap();

    let map: Vec<Vec<char>> = input
        .lines()
        .map(|l| {
            return l.chars().collect();
        })
        .collect();

    writeln!(io::stdout(), "p1: {}", find_shortest_path(&map, false)).unwrap();
    writeln!(io::stdout(), "p2: {}", find_shortest_path(&map, true)).unwrap();
    Ok(())
}
