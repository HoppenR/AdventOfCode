from argparse import ArgumentParser
from typing import Tuple, Dict, List

def spiral(i: int) -> int:
    hor: bool = True
    lineLen: int = 0
    cur: int = 0
    while cur < i:
        lineLen += hor
        cur += lineLen
        hor = not hor
    distx: int = (lineLen-1) // 2 - (cur - i)
    disty: int = lineLen // 2
    return abs(distx) + abs(disty)

def stress_test(i: int) -> int:
    directions: List[Tuple[int, int]] = [(1, 0), (0, 1), (-1, 0), (0, -1)]
    comparisons: List[Tuple[int, int]] = [(1, 0), (0, 1), (-1, 0), (0, -1), (1, 1), (1, -1), (-1, 1), (-1, -1)]
    values: Dict[Tuple[int, int], int] = {(0, 0): 1}
    position: Tuple[int, int] = (0, 0)
    hor: bool = True
    running: bool = True
    lineLen: int = 0
    dirIdx: int = 0
    posVal: int = 0
    while running:
        lineLen += hor
        dirIdx = (dirIdx + 1) % 4
        for _ in range(0, lineLen):
            posVal = 0
            position = pos_sum(position, directions[dirIdx])
            for cOffset in comparisons:
                posVal += values.get(pos_sum(position, cOffset), 0)
            if position not in values:
                if posVal > i:
                    running = False
                    break
                values[position] = posVal
        hor = not hor
    return posVal

def pos_sum(t1: Tuple[int, int], t2: Tuple[int, int]) -> Tuple[int, int]:
    return (sum(i[0] for i in [t1, t2]), sum(i[1] for i in [t1, t2]))

def main() -> int:
    parser = ArgumentParser()
    parser.add_argument('file')
    args = parser.parse_args()
    with open(args.file, encoding='utf-8') as f:
        digit: int = int(f.readline().strip())
        print(spiral(digit))
        print(stress_test(digit))
    return 0

if __name__ == '__main__':
    exit(main())
