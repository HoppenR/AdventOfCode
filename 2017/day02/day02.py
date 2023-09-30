from argparse import ArgumentParser
from typing import Callable, List, Union


def coprime(x: int, y: int) -> int:
    if y > x:
        x, y = y, x
    return x // y if float(x / y).is_integer() else 0


def checksum(
    digs: List[List[int]],
    fn: Union[Callable[[int, int], int], None] = None
) -> int:
    if not fn:
        return sum(max(line) - min(line) for line in digs)
    else:
        return sum(
            fn(digit, odigit)
            for line in digs
            for j, digit in enumerate(line)
            for odigit in line[j+1:]
        )


def main() -> int:
    parser = ArgumentParser()
    parser.add_argument('file')
    args = parser.parse_args()
    with open(args.file, encoding='utf-8') as f:
        digits = [[int(digit) for digit in line.strip().split()]
                  for line in f.readlines()]
        print(checksum(digits))
        print(checksum(digits, coprime))
    return 0


if __name__ == '__main__':
    exit(main())
