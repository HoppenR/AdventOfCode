use std::collections::HashMap;
use std::io::{self, Error, Read, Write};

fn total_folder_sizes(lines: &Vec<&str>) -> HashMap<String, usize> {
    let mut total_sizes: HashMap<String, usize> = HashMap::new();
    let mut path: String = String::new();
    for line in lines {
        let (cmd, arg) = line.rsplit_once(" ").unwrap();
        match cmd {
            "$" | "dir" => {
                continue;
            }
            "$ cd" => match arg {
                ".." => {
                    let lastslash = path.rfind("/").unwrap();
                    path = path[0..lastslash].to_string()
                }
                "/" => {
                    continue;
                }
                _ => {
                    path.push('/');
                    path.push_str(arg);
                }
            },
            _ => {
                for parent in parent_folders(&path.clone()) {
                    *total_sizes.entry(parent).or_insert(0) += cmd.parse::<usize>().unwrap();
                }
            }
        }
    }
    return total_sizes;
}

fn parent_folders(parent_string: &String) -> Vec<String> {
    let mut parents: Vec<String> = Vec::new();
    let mut parent_parts: Vec<&str> = parent_string.split("/").collect();
    while parent_parts.len() != 0 {
        parents.push(parent_parts.join("/"));
        parent_parts.pop();
    }
    return parents;
}

fn filter_sum_dir_tree(lines: &Vec<&str>) -> usize {
    return total_folder_sizes(lines)
        .into_values()
        .filter(|&size| size <= 100000)
        .sum();
}

fn smallest_deletion(lines: &Vec<&str>) -> usize {
    let folder_sizes: HashMap<String, usize> = total_folder_sizes(lines);
    let free_space: usize = 70000000 - folder_sizes[&"".to_string()];
    return folder_sizes
        .into_values()
        .filter(|&size| free_space + size >= 30000000)
        .min()
        .unwrap();
}

fn main() -> Result<(), Error> {
    let mut input: String = String::new();
    io::stdin().read_to_string(&mut input)?;
    let lines: Vec<&str> = parse(&input);
    writeln!(io::stdout(), "p1: {}", filter_sum_dir_tree(&lines))?;
    writeln!(io::stdout(), "p2: {}", smallest_deletion(&lines))?;
    Ok(())
}

fn parse(input: &str) -> Vec<&str> {
    return input.lines().collect();
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE: &str = "\
$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k";
    #[test]
    fn test_part1() {
        assert_eq!(filter_sum_dir_tree(&parse(EXAMPLE)), 95437);
    }

    #[test]
    fn test_part2() {
        assert_eq!(smallest_deletion(&parse(EXAMPLE)), 24933642);
    }
}
