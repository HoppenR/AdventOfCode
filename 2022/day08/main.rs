use std::io::{self, Error, Read, Write};

fn visible_from_perimiter(trees: &Vec<Vec<u32>>) -> usize {
    let mut seen: Vec<Vec<bool>> = trees
        .iter()
        .map(|l| return l.iter().map(|_| false).collect())
        .collect();
    for y in 0..trees.len() {
        let mut left_high: u32 = 0;
        let mut right_high: u32 = 0;
        for x in 0..trees.len() {
            let lefttree: u32 = trees[y][x];
            let righttree: u32 = trees[y][trees.len() - x - 1];
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
    for x in 0..trees.len() {
        let mut up_high: u32 = 0;
        let mut down_high: u32 = 0;
        for y in 0..trees.len() {
            let uptree: u32 = trees[y][x];
            let downtree: u32 = trees[trees.len() - y - 1][x];
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
            return l.into_iter().filter(|b| *b).count();
        })
        .sum();
}

fn outlook_score(ty: usize, tx: usize, trees: &Vec<Vec<u32>>) -> usize {
    let mut scores: Vec<usize> = Vec::from([0; 4]);
    let tree_height: u32 = trees[ty][tx];
    for y in ty + 1..trees.len() {
        scores[0] += 1;
        if trees[y][tx] >= tree_height {
            break;
        }
    }
    for x in (0..tx).rev() {
        scores[1] += 1;
        if trees[ty][x] >= tree_height {
            break;
        }
    }
    for y in (0..ty).rev() {
        scores[2] += 1;
        if trees[y][tx] >= tree_height {
            break;
        }
    }
    for x in tx + 1..trees.len() {
        scores[3] += 1;
        if trees[ty][x] >= tree_height {
            break;
        }
    }
    return scores.into_iter().product();
}

fn best_outlook_tree(trees: &Vec<Vec<u32>>) -> usize {
    let mut best: usize = 0;
    for y in 0..trees.len() {
        for x in 0..trees.len() {
            best = usize::max(best, outlook_score(y, x, trees));
        }
    }
    return best;
}

fn main() -> Result<(), Error> {
    let mut input: String = String::new();
    io::stdin().read_to_string(&mut input)?;
    let trees: Vec<Vec<u32>> = parse(&input);
    writeln!(io::stdout(), "p1: {}", visible_from_perimiter(&trees))?;
    writeln!(io::stdout(), "p2: {}", best_outlook_tree(&trees))?;
    Ok(())
}

fn parse(input: &str) -> Vec<Vec<u32>> {
    return input
        .trim()
        .split("\n")
        .map(|l| {
            return l
                .chars()
                .map(|c| return c.to_digit(10).unwrap() + 1)
                .collect();
        })
        .collect();
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE: &str = "\
30373
25512
65332
33549
35390";
    #[test]
    fn test_part1() {
        assert_eq!(visible_from_perimiter(&parse(EXAMPLE)), 21);
    }

    #[test]
    fn test_part2() {
        assert_eq!(best_outlook_tree(&parse(EXAMPLE)), 8);
    }
}
