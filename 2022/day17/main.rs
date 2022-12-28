use std::collections::HashSet;
use std::io::{self, Error, Read, Write};

enum Direction {
    Left,
    Right,
    Down,
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

fn tetris(directions: &Vec<Direction>, max_rocks: usize) -> usize {
    let shapes: Vec<&str> = vec![
        "####",
        ".#.\n###\n.#.",
        "..#\n..#\n###",
        "#\n#\n#\n#",
        "##\n##",
    ];
    let mut board: HashSet<(usize, usize)> = HashSet::new();
    let mut shape_index: usize = 0;
    let mut direction_index: usize = 0;
    let mut stack_height: usize = 0;
    for _ in 0..max_rocks {
        let shape_height: usize = shapes[shape_index].chars().filter(|c| *c == '\n').count() + 1;
        let mut posy: usize = stack_height + shape_height + 3;
        let mut posx: usize = 3;
        let current_shape: &str = shapes[shape_index];
        loop {
            let current_dir: &Direction = &directions[direction_index];
            if !collides(current_shape, &board, posx, posy, current_dir) {
                match current_dir {
                    Direction::Left => posx -= 1,
                    Direction::Right => posx += 1,
                    Direction::Down => {}
                }
            }
            direction_index = (direction_index + 1) % directions.len();
            if collides(current_shape, &board, posx, posy, &Direction::Down) {
                break;
            }
            posy -= 1;
        }
        insert_shape(current_shape, &mut board, posx, posy);
        stack_height = usize::max(stack_height, posy);
        shape_index = (shape_index + 1) % shapes.len();
    }
    return stack_height;
}

fn main() -> Result<(), Error> {
    let mut input: String = String::new();
    io::stdin().read_to_string(&mut input).unwrap();

    let directions: Vec<Direction> = input
        .trim()
        .chars()
        .filter_map(|c| match c {
            '<' => return Some(Direction::Left),
            '>' => return Some(Direction::Right),
            _ => return None,
        })
        .collect();

    writeln!(io::stdout(), "p1: {}", tetris(&directions, 2022)).unwrap();
    Ok(())
}
