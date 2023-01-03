use std::collections::VecDeque;
use std::io::{self, Error, Read, Write};

#[derive(Hash, Eq, PartialEq, Clone)]
struct Point {
    x: i32,
    y: i32,
}

fn find(map: &Vec<Vec<char>>, ch: char) -> Point {
    for y in 0..map.len() {
        for x in 0..map[0].len() {
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
    let mut queue: VecDeque<(Point, usize)> = VecDeque::new();
    let mut seen: VecDeque<Point> = VecDeque::new();
    let checks: [Point; 4] = [
        Point { y: 1, x: 0 },
        Point { y: 0, x: -1 },
        Point { y: -1, x: 0 },
        Point { y: 0, x: 1 },
    ];
    queue.push_back((start, 0));
    while !queue.is_empty() {
        let (cur_point, cost): (Point, usize) = queue.pop_front().unwrap();
        let cur_elevation: char = map[cur_point.y as usize][cur_point.x as usize];
        if (!reverse && cur_elevation == 'E') || (reverse && cur_elevation == 'a') {
            return cost;
        }
        for check in &checks {
            let comp = Point {
                y: cur_point.y + check.y,
                x: cur_point.x + check.x,
            };
            if let Some(&target_elevation) = map
                .get(comp.y as usize)
                .and_then(|line| line.get(comp.x as usize))
            {
                if (!reverse && can_move(cur_elevation, target_elevation))
                    || (reverse && can_move(target_elevation, cur_elevation))
                {
                    if !seen.contains(&comp) {
                        seen.push_back(comp.clone());
                        queue.push_back((comp.clone(), cost + 1));
                    }
                }
            }
        }
    }
    unreachable!();
}

fn can_move(from: char, to: char) -> bool {
    if from == 'S' {
        return to == 'a';
    } else if to == 'E' {
        return from == 'z';
    } else if to <= from {
        return true;
    } else {
        return to as u32 - from as u32 == 1;
    }
}

fn main() -> Result<(), Error> {
    let mut input: String = String::new();
    io::stdin().read_to_string(&mut input)?;
    let map: Vec<Vec<char>> = parse(&input);
    writeln!(io::stdout(), "p1: {}", find_shortest_path(&map, false))?;
    writeln!(io::stdout(), "p2: {}", find_shortest_path(&map, true))?;
    Ok(())
}

fn parse(input: &str) -> Vec<Vec<char>> {
    return input
        .lines()
        .map(|l| {
            return l.chars().collect();
        })
        .collect();
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE: &str = "\
Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi";
    #[test]
    fn test_part1() {
        assert_eq!(find_shortest_path(&parse(EXAMPLE), false), 31);
    }

    #[test]
    fn test_part2() {
        assert_eq!(find_shortest_path(&parse(EXAMPLE), true), 29);
    }
}
