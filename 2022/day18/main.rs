use std::{
    collections::HashSet,
    io::{self, Error, Read, Write},
};

#[derive(Debug, Hash, Eq, PartialEq)]
struct Point3D {
    x: i32,
    y: i32,
    z: i32,
}

fn get_droplet_sizes(droplets: &Vec<Point3D>) -> HashSet<Point3D> {
    let mut sides: HashSet<Point3D> = HashSet::new();
    let compare_offsets: Vec<(i32, i32, i32)> = vec![
        (1, 0, 0),
        (0, 1, 0),
        (0, 0, 1),
        (-1, 0, 0),
        (0, -1, 0),
        (0, 0, -1),
    ];
    for drop in droplets {
        for comp_off in &compare_offsets {
            let cur_side = Point3D {
                x: drop.x + comp_off.0,
                y: drop.y + comp_off.1,
                z: drop.z + comp_off.2,
            };
            if sides.contains(&cur_side) {
                sides.remove(&cur_side);
            } else {
                sides.insert(cur_side);
            }
        }
    }
    return sides;
}

fn count_surface_area(droplets: &Vec<Point3D>) -> usize {
    return get_droplet_sizes(droplets).len();
}

fn main() -> Result<(), Error> {
    let mut input: String = String::new();
    io::stdin().read_to_string(&mut input).unwrap();

    let droplets: Vec<Point3D> = input
        .trim()
        .split('\n')
        .map(|line| {
            let coords: Vec<&str> = line.splitn(3, ',').collect();
            return Point3D {
                x: coords[0].parse::<i32>().unwrap() * 2,
                y: coords[1].parse::<i32>().unwrap() * 2,
                z: coords[2].parse::<i32>().unwrap() * 2,
            };
        })
        .collect::<Vec<Point3D>>();

    writeln!(io::stdout(), "p1: {}", count_surface_area(&droplets)).unwrap();
    Ok(())
}
