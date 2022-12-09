use std::cmp::min;
use std::collections::HashMap;
use std::io::{self, Error, Read, Write};

struct File {
    name: String,
    size: usize,
}

fn file_locations(lines: &Vec<String>) -> Vec<File> {
    let mut files: Vec<File> = Vec::new();
    let mut path: String = "".to_string();
    for line in lines {
        let (cmd, arg) = line.rsplit_once(" ").unwrap();
        match cmd {
            "$" | "dir" => continue,
            "$ cd" => match arg {
                ".." => path = path[0..path.rfind("/").unwrap()].to_string(),
                "/" => continue,
                _ => {
                    path.push('/');
                    path.push_str(arg);
                }
            },
            _ => files.push(File {
                name: path.clone(),
                size: cmd.parse().unwrap(),
            }),
        }
    }
    return files;
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

fn files_to_total_folder_sizes(files: &Vec<File>) -> HashMap<String, usize> {
    let mut total_sizes: HashMap<String, usize> = HashMap::new();
    for file in files {
        for parent in parent_folders(&file.name) {
            *total_sizes.entry(parent).or_insert(0) += file.size;
        }
    }
    return total_sizes;
}

fn sum_file_sizes_capped(total_sizes: &HashMap<String, usize>, max: usize) -> usize {
    return total_sizes
        .iter()
        .map(|(_, size)| size)
        .filter(|&size| size <= &max)
        .sum();
}

fn filter_sum_dir_tree(lines: &Vec<String>) -> usize {
    let files: Vec<File> = file_locations(lines);
    let total_sizes: HashMap<String, usize> = files_to_total_folder_sizes(&files);
    let sum: usize = sum_file_sizes_capped(&total_sizes, 100000);
    return sum;
}

fn smallest_required_deletion(lines: &Vec<String>) -> usize {
    let files: Vec<File> = file_locations(lines);
    let total_sizes: HashMap<String, usize> = files_to_total_folder_sizes(&files);
    let free_space: usize = 70000000 - total_sizes[&"".to_string()];
    let mut smallest_deletion: usize = usize::MAX;
    for (_, size) in total_sizes {
        if free_space + size >= 30000000 {
            smallest_deletion = min(smallest_deletion, size);
        }
    }
    return smallest_deletion;
}

fn main() -> Result<(), Error> {
    let mut input: String = String::new();
    io::stdin().read_to_string(&mut input).unwrap();

    let lines: Vec<String> = input.lines().map(String::from).collect();

    writeln!(io::stdout(), "p1: {}", filter_sum_dir_tree(&lines)).unwrap();
    writeln!(io::stdout(), "p2: {}", smallest_required_deletion(&lines)).unwrap();
    Ok(())
}
