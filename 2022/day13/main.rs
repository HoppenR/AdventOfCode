use std::cmp;
use std::io::{self, Error, Read, Write};
use std::str::Chars;

#[derive(Clone, PartialEq)]
enum PackT {
    List,
    Num,
}

#[derive(Clone)]
struct Packet {
    children: Vec<Packet>,
    t: PackT,
    value: u32,
}

impl Packet {
    fn new_list(children: Vec<Packet>) -> Self {
        return Packet {
            children,
            t: PackT::List,
            value: 0,
        };
    }

    fn new_num(value: u32) -> Self {
        return Packet {
            children: Vec::new(),
            t: PackT::Num,
            value,
        };
    }
}

fn num_to_list(num_packet: &Packet) -> Packet {
    return Packet::new_list(vec![Packet::new_num(num_packet.value)]);
}

fn compare_packets(lhs: &Packet, rhs: &Packet) -> cmp::Ordering {
    if lhs.t == PackT::List && rhs.t == PackT::List {
        for i in 0..cmp::min(lhs.children.len(), rhs.children.len()) {
            let comp = compare_packets(&lhs.children[i], &rhs.children[i]);
            if comp.is_ne() {
                return comp;
            }
        }
        return lhs.children.len().cmp(&rhs.children.len());
    } else if lhs.t == PackT::Num && rhs.t == PackT::Num {
        return lhs.value.cmp(&rhs.value);
    } else if lhs.t == PackT::Num && rhs.t == PackT::List {
        return compare_packets(&num_to_list(lhs), rhs);
    } else {
        return compare_packets(lhs, &num_to_list(rhs));
    }
}

fn sorted_packets(packets: &Vec<Packet>) -> usize {
    let mut index_count: usize = 0;
    for (i, pair) in packets.windows(2).step_by(2).enumerate() {
        if compare_packets(&pair[0], &pair[1]).is_lt() {
            index_count += i + 1;
        }
    }
    return index_count;
}

fn decoder_key(old_packets: &Vec<Packet>) -> usize {
    let mut packets = old_packets.clone();
    let mut product: usize = 1;
    packets.push(parse_root_packet(&mut "[[2]]".chars()));
    packets.push(parse_root_packet(&mut "[[6]]".chars()));
    packets.sort_by(compare_packets);
    for (i, packet) in packets.iter().enumerate() {
        if is_divider_packet(&packet) {
            product *= i + 1
        }
    }
    return product;
}

fn is_divider_packet(root: &Packet) -> bool {
    let mut packet = root;
    for _ in 0..2 {
        if packet.children.len() != 1 {
            return false;
        }
        packet = &packet.children[0];
    }
    match packet.value {
        2 => return true,
        6 => return true,
        _ => return false,
    }
}

fn parse_packet(ch_iter: &mut Chars) -> Packet {
    let mut number_str: String = String::new();
    let mut packet: Packet = Packet::new_list(Vec::new());
    loop {
        let next_c = ch_iter.next().unwrap();
        match next_c {
            '[' => {
                packet.children.push(parse_packet(ch_iter));
            }
            ']' | ',' => {
                if number_str.len() > 0 {
                    packet
                        .children
                        .push(Packet::new_num(number_str.parse().unwrap()));
                }
                if next_c == ']' {
                    return packet;
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
        .map(|pair| {
            return pair.trim().split("\n").map(|l| {
                return parse_root_packet(&mut l.chars());
            });
        })
        .flatten()
        .collect();

    writeln!(io::stdout(), "p1: {}", sorted_packets(&packets)).unwrap();
    writeln!(io::stdout(), "p2: {}", decoder_key(&packets)).unwrap();
    Ok(())
}
