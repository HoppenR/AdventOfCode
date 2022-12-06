use std::collections::HashSet;
use std::io::{self, Error, Read, Write};
use std::iter::FromIterator;

fn n_first_unique_at(data: &String, unique_len: usize) -> usize {
    for startpos in 0..data.len() - unique_len {
        let data_span = data.chars().skip(startpos).take(unique_len);
        let unique_data: HashSet<char> = HashSet::from_iter(data_span);
        if unique_data.len() == unique_len {
            return startpos + unique_len;
        }
    }
    return 0;
}

fn main() -> Result<(), Error> {
    let mut input: String = String::new();
    io::stdin().read_to_string(&mut input).unwrap();

    input.pop(); // Remove '\n'

    writeln!(io::stdout(), "p1: {}", n_first_unique_at(&input, 4)).unwrap();
    writeln!(io::stdout(), "p2: {}", n_first_unique_at(&input, 14)).unwrap();
    Ok(())
}
