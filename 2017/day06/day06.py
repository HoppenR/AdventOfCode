from argparse import ArgumentParser
from typing import List, Dict

def redist_cycles(mem_banks: List[int], debug_mode: int) -> int:
    seen: Dict[str, int] = {}
    i: int = 0
    while True:
        key: str = str(mem_banks)
        if key in seen:
            if debug_mode == 1:
                return len(seen)
            elif debug_mode == 2:
                return i - seen[key]
        seen[key] = i
        block_amount: int = max(mem_banks)
        block_idx: int = mem_banks.index(block_amount)
        mem_banks[block_idx] = 0
        while block_amount > 0:
            block_idx += 1
            mem_banks[block_idx % len(mem_banks)] += 1
            block_amount -= 1
        i += 1

def main() -> int:
    parser = ArgumentParser()
    parser.add_argument('file')
    args = parser.parse_args()
    with open(args.file, encoding='utf-8') as f:
        mem_banks: List[int] = [int(digit) for digit in f.readline().strip().split('\t')]
        print(redist_cycles(mem_banks, 1))
        print(redist_cycles(mem_banks, 2))
    return 0

if __name__ == '__main__':
    exit(main())
