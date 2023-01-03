use std::collections::HashSet;
use std::io::{self, Error, Read, Write};

fn unique_span(data: &str, unique_len: usize) -> usize {
    return data
        .as_bytes()
        .windows(unique_len)
        .position(|data_span| {
            return data_span.iter().collect::<HashSet<&u8>>().len() == unique_len;
        })
        .unwrap()
        + unique_len;
}

fn main() -> Result<(), Error> {
    let mut input: String = String::new();
    io::stdin().read_to_string(&mut input)?;
    writeln!(io::stdout(), "p1: {}", unique_span(parse(&input), 4))?;
    writeln!(io::stdout(), "p2: {}", unique_span(parse(&input), 14))?;
    Ok(())
}

fn parse(input: &str) -> &str {
    return input.trim();
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLES: [&str; 5] = [
        "mjqjpqmgbljsphdztnvjfqwrcgsmlb",
        "bvwbjplbgvbhsrlpgdmjqwftvncz",
        "nppdvjthqldpwncqszvftbrmjlhg",
        "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
        "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw",
    ];
    #[test]
    fn test_part1() {
        assert_eq!(unique_span(parse(EXAMPLES[0]), 4), 7);
        assert_eq!(unique_span(parse(EXAMPLES[1]), 4), 5);
        assert_eq!(unique_span(parse(EXAMPLES[2]), 4), 6);
        assert_eq!(unique_span(parse(EXAMPLES[3]), 4), 10);
        assert_eq!(unique_span(parse(EXAMPLES[4]), 4), 11);
    }

    #[test]
    fn test_part2() {
        assert_eq!(unique_span(parse(EXAMPLES[0]), 14), 19);
        assert_eq!(unique_span(parse(EXAMPLES[1]), 14), 23);
        assert_eq!(unique_span(parse(EXAMPLES[2]), 14), 23);
        assert_eq!(unique_span(parse(EXAMPLES[3]), 14), 29);
        assert_eq!(unique_span(parse(EXAMPLES[4]), 14), 26);
    }
}
