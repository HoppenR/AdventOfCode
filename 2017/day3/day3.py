from argparse import ArgumentParser
from typing import Callable, List, Optional
from math import gcd

def spiral(i: int) -> int:
    hor = True
    lineLen = cur = 0
    while cur < i:
        lineLen += hor
        cur += lineLen
        hor = not hor
    distx = (lineLen-1) // 2 - (cur - i)
    disty = lineLen // 2
    return abs(distx) + abs(disty)

def main() -> int:
    parser = ArgumentParser()
    parser.add_argument('file')
    args = parser.parse_args()
    with open(args.file, encoding='utf-8') as f:
        digit = int(f.readline().strip())
        print(spiral(digit))
    return 0

if __name__ == '__main__':
    exit(main())
