from argparse import ArgumentParser
from typing import List

def count_sequence(digs: List[int], offset: int) -> int:
    return sum(s for i, s in enumerate(digs) if s == digs[(i+offset)%len(digs)])

def main() -> int:
    parser = ArgumentParser()
    parser.add_argument('file')
    args = parser.parse_args()
    with open(args.file, encoding='utf-8') as f:
        digits = [int(byte) for byte in f.read().strip()]
        print(count_sequence(digits, 1))
        print(count_sequence(digits, len(digits)//2))
    return 0

if __name__ == '__main__':
    exit(main())
