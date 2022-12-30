use std::cmp;
use std::io::{self, Error, Read, Write};
use std::str::Chars;

#[derive(Clone, Eq, PartialEq)]
enum Packet {
    List(Vec<Packet>),
    Value(u32),
}

impl PartialOrd for Packet {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        return Some(self.cmp(other));
    }
}

impl Ord for Packet {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        match (self, other) {
            (Packet::List(lhs), Packet::List(rhs)) => {
                return lhs.cmp(rhs);
            }
            (Packet::Value(lhs), Packet::Value(rhs)) => {
                return lhs.cmp(rhs);
            }
            (Packet::Value(lhs), Packet::List(_)) => {
                return Packet::List(vec![Packet::Value(*lhs)]).cmp(other);
            }
            (Packet::List(_), Packet::Value(rhs)) => {
                return self.cmp(&Packet::List(vec![Packet::Value(*rhs)]));
            }
        }
    }
}

fn sorted_packets(packets: &Vec<Packet>) -> usize {
    return packets
        .windows(2)
        .step_by(2)
        .enumerate()
        .filter(|(_, pair)| pair[0] < pair[1])
        .map(|(i, _)| i + 1)
        .sum();
}

fn decoder_key(old_packets: &Vec<Packet>) -> usize {
    let mut packets: Vec<Packet> = old_packets.clone();
    let dividers: [Packet; 2] = [
        parse_root_packet(&mut "[[2]]".chars()),
        parse_root_packet(&mut "[[6]]".chars()),
    ];
    packets.extend_from_slice(&dividers);
    packets.sort();
    return dividers
        .iter()
        .filter_map(|divider| packets.binary_search(divider).ok())
        .fold(1, |acc, ix| acc * (ix + 1));
}

fn parse_packet(ch_iter: &mut Chars) -> Packet {
    let mut number_str: String = String::new();
    let mut packet: Vec<Packet> = Vec::new();
    loop {
        let next_c: char = ch_iter.next().unwrap();
        match next_c {
            '[' => {
                packet.push(parse_packet(ch_iter));
            }
            ']' | ',' => {
                if number_str.len() > 0 {
                    packet.push(Packet::Value(number_str.parse().unwrap()));
                }
                if next_c == ']' {
                    return Packet::List(packet);
                }
                number_str.clear();
            }
            _ => {
                number_str.push(next_c);
            }
        }
    }
}

fn parse_root_packet(ch_iter: &mut Chars) -> Packet {
    ch_iter.next(); // Skip leading '['
    return parse_packet(ch_iter);
}

fn main() -> Result<(), Error> {
    let mut input: String = String::new();
    io::stdin().read_to_string(&mut input).unwrap();

    let packets: Vec<Packet> = input
        .split("\n\n")
        .flat_map(|pair| {
            return pair.trim().split("\n").map(|l| {
                return parse_root_packet(&mut l.chars());
            });
        })
        .collect();

    writeln!(io::stdout(), "p1: {}", sorted_packets(&packets)).unwrap();
    writeln!(io::stdout(), "p2: {}", decoder_key(&packets)).unwrap();
    Ok(())
}
