from argparse import ArgumentParser
from typing import List
from copy import deepcopy


def run(program: List[int], mode: int) -> int:
    pc: int = 0
    steps: int = 0
    while pc < len(program):
        offset: int = program[pc]
        if (mode == 2 and program[pc] < 3) or mode == 1:
            program[pc] += 1
        elif mode == 2 and program[pc] >= 3:
            program[pc] -= 1
        pc += offset
        steps += 1
    return steps


def main() -> int:
    parser = ArgumentParser()
    parser.add_argument('file')
    args = parser.parse_args()
    with open(args.file, encoding='utf-8') as f:
        program: List[int] = [int(d) for d in f.readlines()]
        p1: List[int] = deepcopy(program)
        p2: List[int] = deepcopy(program)
        print(run(p1, 1))
        print(run(p2, 2))
    return 0


if __name__ == '__main__':
    exit(main())
