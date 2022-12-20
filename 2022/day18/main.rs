use std::collections::{HashSet, VecDeque};
use std::io::{self, Error, Read, Write};

fn neighbors((x, y, z): (i32, i32, i32)) -> Vec<(i32, i32, i32)> {
    return vec![
        (x + 1, y, z),
        (x, y + 1, z),
        (x, y, z + 1),
        (x - 1, y, z),
        (x, y - 1, z),
        (x, y, z - 1),
    ];
}

fn flood_fill_outer(points: &Vec<(i32, i32, i32)>) -> HashSet<(i32, i32, i32)> {
    let (mut min_x, mut min_y, mut min_z, mut max_x, mut max_y, mut max_z) =
        (i32::MAX, i32::MAX, i32::MAX, i32::MIN, i32::MIN, i32::MIN);
    for &(x, y, z) in points {
        min_x = i32::min(min_x, x);
        min_y = i32::min(min_y, y);
        min_z = i32::min(min_z, z);
        max_x = i32::max(max_x, x);
        max_y = i32::max(max_y, y);
        max_z = i32::max(max_z, z);
    }
    let mut seen: HashSet<(i32, i32, i32)> = HashSet::new();
    let mut queue: VecDeque<(i32, i32, i32)> = VecDeque::new();
    let start: (i32, i32, i32) = (0, 0, 0);
    queue.push_back(start);
    seen.insert(start);
    while queue.len() > 0 {
        let point = queue.pop_front().unwrap();
        for (x, y, z) in neighbors(point) {
            if (x >= min_x - 1 && x <= max_x + 1)
                && (y >= min_y - 1 && y <= max_y + 1)
                && (z >= min_z - 1 && z <= max_z + 1)
                && !points.contains(&(x, y, z))
                && seen.insert((x, y, z))
            {
                queue.push_back((x, y, z));
            }
        }
    }
    return seen;
}

fn all_sides(points: &Vec<(i32, i32, i32)>) -> usize {
    return points
        .iter()
        .flat_map(|&p| neighbors(p))
        .filter(|p| !points.contains(p))
        .count();
}

fn extern_sides(points: &Vec<(i32, i32, i32)>) -> usize {
    return flood_fill_outer(points)
        .iter()
        .flat_map(|&p| neighbors(p))
        .filter(|p| points.contains(p))
        .count();
}

fn main() -> Result<(), Error> {
    let mut input: String = String::new();
    io::stdin().read_to_string(&mut input).unwrap();

    let droplets: Vec<(i32, i32, i32)> = input
        .trim()
        .split('\n')
        .map(|line| {
            let coords: Vec<&str> = line.splitn(3, ',').collect();
            return (
                coords[0].parse().unwrap(),
                coords[1].parse().unwrap(),
                coords[2].parse().unwrap(),
            );
        })
        .collect();

    writeln!(io::stdout(), "p1: {}", all_sides(&droplets)).unwrap();
    writeln!(io::stdout(), "p2: {}", extern_sides(&droplets)).unwrap();
    Ok(())
}
