use std::io::{self, Error, Read, Write};

fn visible_from_perimiter(trees: &Vec<Vec<u32>>) -> usize {
    let mut seen: Vec<Vec<bool>> = trees
        .iter()
        .map(|l| return l.iter().map(|_| false).collect())
        .collect();
    // Left <-> Right
    for y in 0..trees.len() {
        let mut left_high = 0;
        let mut right_high = 0;
        for x in 0..trees.len() {
            let lefttree = trees[y][x];
            let righttree = trees[y][trees.len() - x - 1];
            if lefttree > left_high {
                left_high = lefttree;
                seen[y][x] = true;
            }
            if righttree > right_high {
                right_high = righttree;
                seen[y][trees.len() - x - 1] = true;
            }
        }
    }
    // Up <-> Down
    for x in 0..trees.len() {
        let mut up_high = 0;
        let mut down_high = 0;
        for y in 0..trees.len() {
            let uptree = trees[y][x];
            let downtree = trees[trees.len() - y - 1][x];
            if uptree > up_high {
                up_high = uptree;
                seen[y][x] = true;
            }
            if downtree > down_high {
                down_high = downtree;
                seen[trees.len() - y - 1][x] = true;
            }
        }
    }
    return seen
        .into_iter()
        .map(|l| {
            return l.into_iter().map(|b| b as usize).sum::<usize>();
        })
        .sum();
}

fn outlook_score(ty: usize, tx: usize, trees: &Vec<Vec<u32>>) -> usize {
    let mut scores: Vec<usize> = Vec::new();
    scores.resize(4, 0);
    let theight = trees[ty][tx];
    // Up
    for y in ty + 1..trees.len() {
        scores[0] += 1;
        if trees[y][tx] >= theight {
            break;
        }
    }
    // Left
    for x in (0..tx).rev() {
        scores[1] += 1;
        if trees[ty][x] >= theight {
            break;
        }
    }
    // Down
    for y in (0..ty).rev() {
        scores[2] += 1;
        if trees[y][tx] >= theight {
            break;
        }
    }
    // Right
    for x in tx + 1..trees.len() {
        scores[3] += 1;
        if trees[ty][x] >= theight {
            break;
        }
    }
    return scores.into_iter().reduce(|a, b| a * b).unwrap();
}

fn best_outlook_tree(trees: &Vec<Vec<u32>>) -> usize {
    let mut max = 0;
    for y in 0..trees.len() {
        for x in 0..trees.len() {
            let score = outlook_score(y, x, trees);
            if score > max {
                max = score;
            }
        }
    }
    return max;
}

fn main() -> Result<(), Error> {
    let mut input: String = String::new();
    io::stdin().read_to_string(&mut input).unwrap();

    let trees: Vec<Vec<u32>> = input
        .trim()
        .split("\n")
        .map(|l| {
            return l
                .chars()
                .map(|c| return c.to_digit(10).unwrap() + 1)
                .collect();
        })
        .collect();

    writeln!(io::stdout(), "p1: {}", visible_from_perimiter(&trees)).unwrap();
    writeln!(io::stdout(), "p2: {}", best_outlook_tree(&trees)).unwrap();
    Ok(())
}
