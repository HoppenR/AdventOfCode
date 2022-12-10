use std::collections::HashMap;
use std::io::{self, Error, Read, Write};

fn total_folder_sizes(lines: &Vec<String>) -> HashMap<String, usize> {
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

fn filter_sum_dir_tree(lines: &Vec<String>) -> usize {
    return total_folder_sizes(lines)
        .into_values()
        .filter(|&size| size <= 100000)
        .sum();
}

fn smallest_required_deletion(lines: &Vec<String>) -> usize {
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
    io::stdin().read_to_string(&mut input).unwrap();

    let lines: Vec<String> = input.lines().map(String::from).collect();

    writeln!(io::stdout(), "p1: {}", filter_sum_dir_tree(&lines)).unwrap();
    writeln!(io::stdout(), "p2: {}", smallest_required_deletion(&lines)).unwrap();
    Ok(())
}
