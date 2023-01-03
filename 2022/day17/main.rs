use std::collections::{HashMap, HashSet};
use std::io::{self, Error, Read, Write};

enum Direction {
    Left,
    Right,
    Down,
}

#[derive(Hash, Eq, PartialEq)]
struct State {
    cur_direction_index: usize,
    landed_x: usize,
    shape_index: usize,
    top_y_offsets: [usize; 7],
}

fn top_down_view(board: &HashSet<(usize, usize)>, stack_height: usize) -> [usize; 7] {
    let mut top_y_offsets: [usize; 7] = [usize::MAX; 7];
    for x in 1..=7 {
        for y_off in 0..stack_height {
            if board.contains(&(stack_height - y_off, x)) {
                top_y_offsets[x - 1] = y_off;
                break;
            }
        }
    }
    return top_y_offsets;
}

fn collides(
    shape: &str,
    board: &HashSet<(usize, usize)>,
    posx: usize,
    posy: usize,
    direction: &Direction,
) -> bool {
    match direction {
        Direction::Left => {
            if posx == 1 {
                return true;
            }
            return shape.lines().enumerate().any(|(i, l)| {
                return board.contains(&(posy - i, posx + l.find('#').unwrap() - 1));
            });
        }
        Direction::Right => {
            let width = shape.lines().next().unwrap().len();
            if (posx + width - 1) == 7 {
                return true;
            }
            return shape.lines().enumerate().any(|(i, l)| {
                return board.contains(&(posy - i, posx + l.rfind('#').unwrap() + 1));
            });
        }
        Direction::Down => {
            if posy - 1 == 0 {
                return true;
            }
            return shape.lines().enumerate().any(|(y, line)| {
                return line.chars().enumerate().any(|(x, ch)| {
                    return ch == '#' && board.contains(&(posy - y - 1, posx + x));
                });
            });
        }
    }
}

fn insert_shape(shape: &str, board: &mut HashSet<(usize, usize)>, posx: usize, posy: usize) {
    for (y, l) in shape.lines().enumerate() {
        for (x, c) in l.chars().enumerate() {
            if c == '#' {
                board.insert((posy - y, posx + x));
            }
        }
    }
}

fn tetris(dirs: &Vec<Direction>, max_rocks: usize) -> usize {
    let shapes: [&str; 5] = [
        "####",
        ".#.\n###\n.#.",
        "..#\n..#\n###",
        "#\n#\n#\n#",
        "##\n##",
    ];
    let mut board: HashSet<(usize, usize)> = HashSet::new();
    let mut states: HashMap<State, (usize, usize)> = HashMap::new();
    let mut dir_idx: usize = 0;
    let mut dropped_rocks = 0;
    let mut shape_idx: usize = 0;
    let mut stack_height: usize = 0;
    let mut total_cycle_height: usize = 0;
    while dropped_rocks < max_rocks {
        let shape_height: usize = shapes[shape_idx].chars().filter(|c| *c == '\n').count() + 1;
        let mut posy: usize = stack_height + shape_height + 3 + 1;
        let mut posx: usize = 3;
        while !collides(shapes[shape_idx], &board, posx, posy, &Direction::Down) {
            posy -= 1;
            if !collides(shapes[shape_idx], &board, posx, posy, &dirs[dir_idx]) {
                match &dirs[dir_idx] {
                    Direction::Left => posx -= 1,
                    Direction::Right => posx += 1,
                    Direction::Down => {}
                }
            }
            dir_idx = (dir_idx + 1) % dirs.len();
        }
        insert_shape(shapes[shape_idx], &mut board, posx, posy);
        dropped_rocks += 1;
        stack_height = usize::max(stack_height, posy);
        shape_idx = (shape_idx + 1) % shapes.len();
        let current_state: State = State {
            landed_x: posx,
            shape_index: shape_idx,
            cur_direction_index: dir_idx,
            top_y_offsets: top_down_view(&board, stack_height),
        };
        if let Some((cycle_height, cycle_rocks)) =
            states.insert(current_state, (stack_height, dropped_rocks))
        {
            let cycle_amounts = (max_rocks - dropped_rocks) / (dropped_rocks - cycle_rocks);
            total_cycle_height += (stack_height - cycle_height) * cycle_amounts;
            dropped_rocks += cycle_amounts * (dropped_rocks - cycle_rocks);
        }
    }
    return stack_height + total_cycle_height;
}

fn main() -> Result<(), Error> {
    let mut input: String = String::new();
    io::stdin().read_to_string(&mut input)?;
    let dirs: Vec<Direction> = parse(&input);
    writeln!(io::stdout(), "p1: {}", tetris(&dirs, 2022))?;
    writeln!(io::stdout(), "p2: {}", tetris(&dirs, 1000000000000))?;
    Ok(())
}

fn parse(input: &str) -> Vec<Direction> {
    return input
        .trim()
        .chars()
        .filter_map(|c| match c {
            '<' => return Some(Direction::Left),
            '>' => return Some(Direction::Right),
            _ => return None,
        })
        .collect();
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE: &str = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>";
    #[test]
    fn test_part1() {
        assert_eq!(tetris(&parse(EXAMPLE), 2022), 3068);
    }

    #[test]
    fn test_part2() {
        assert_eq!(tetris(&parse(EXAMPLE), 1000000000000), 1514285714288);
    }
}
