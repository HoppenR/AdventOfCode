use std::convert::TryInto;
use std::io::{self, Error, Read, Write};

#[derive(Debug, Hash)]
struct Pair {
    sensor: (i32, i32),
    beacon: (i32, i32),
    radius: i32,
}

fn is_inside_circles(buildings: &Vec<Pair>, point: (i32, i32)) -> bool {
    for pair in buildings {
        let dist: i32 = (i32::abs_diff(pair.sensor.0, point.0)
            + i32::abs_diff(pair.sensor.1, point.1))
        .try_into()
        .unwrap();
        if dist <= pair.radius {
            return true;
        }
    }
    return false;
}

fn n_nonbeacons(buildings: &Vec<Pair>, y: i32) -> usize {
    let (mut world_min, mut world_max) = (i32::MAX, i32::MIN);
    for pair in buildings {
        world_min = i32::min(world_min, pair.sensor.0 - pair.radius);
        world_max = i32::max(world_max, pair.sensor.0 + pair.radius);
    }
    let mut count = 0;
    for x in world_min..=world_max {
        if is_inside_circles(&buildings, (x, y)) {
            if !buildings.iter().any(|p| p.beacon == (x, y)) {
                count += 1;
            }
        }
    }
    return count;
}

fn find_distress_beacon(buildings: &Vec<Pair>) -> usize {
    let (world_min, world_max) = (0, 4000000);
    for pair in buildings {
        for line in 0..=pair.radius {
            for c in vec![
                (pair.sensor.0 + pair.radius + 1 - line, pair.sensor.1 + line),
                (pair.sensor.0 + pair.radius + 1 - line, pair.sensor.1 - line),
                (pair.sensor.0 - pair.radius + 1 - line, pair.sensor.1 + line),
                (pair.sensor.0 - pair.radius + 1 - line, pair.sensor.1 - line),
            ] {
                if (c.0 < world_min || c.0 > world_max) || (c.1 < world_min || c.1 > world_max) {
                    continue;
                }
                if !is_inside_circles(buildings, c) {
                    return TryInto::<usize>::try_into(c.0).unwrap() * 4000000usize
                        + TryInto::<usize>::try_into(c.1).unwrap();
                }
            }
        }
    }
    unreachable!();
}

fn main() -> Result<(), Error> {
    let mut input: String = String::new();
    io::stdin().read_to_string(&mut input).unwrap();

    let buildings: Vec<Pair> = input
        .lines()
        .map(|l| {
            let mut iter = l.split(" ").into_iter().skip(2);
            let sensor_x = iter.next().unwrap();
            let sensor_y = iter.next().unwrap();
            let mut iter = iter.skip(4);
            let beacon_x = iter.next().unwrap();
            let beacon_y = iter.next().unwrap();
            let mut pair = Pair {
                sensor: (
                    sensor_x[2..sensor_x.len() - 1].parse().unwrap(),
                    sensor_y[2..sensor_y.len() - 1].parse().unwrap(),
                ),
                beacon: (
                    beacon_x[2..beacon_x.len() - 1].parse().unwrap(),
                    beacon_y[2..beacon_y.len() - 0].parse().unwrap(),
                ),
                radius: 0,
            };
            pair.radius = (i32::abs_diff(pair.sensor.0, pair.beacon.0)
                + i32::abs_diff(pair.sensor.1, pair.beacon.1))
            .try_into()
            .unwrap();
            return pair;
        })
        .collect();

    writeln!(io::stdout(), "p1: {}", n_nonbeacons(&buildings, 2000000)).unwrap();
    writeln!(io::stdout(), "p2: {}", find_distress_beacon(&buildings)).unwrap();
    Ok(())
}
