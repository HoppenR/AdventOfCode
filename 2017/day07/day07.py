from argparse import ArgumentParser
from typing import Dict, List, Tuple


class Disc:
    name: str
    weight: int
    dependencies: List[str]

    def __init__(self):
        self.dependencies = []


def delve(discname: str, discs: Dict[str, Disc]) -> Tuple[int, bool]:
    disc: Disc = discs[discname]
    weights: Dict[str, int] = {}
    for child in disc.dependencies:
        weights[child], has_found = delve(child, discs)
        if has_found:
            return weights[child], True
    last: int = 0
    for name, weight in sorted(weights.items(), key=lambda x: x[1]):
        if last == 0:
            last = weight
        else:
            if weight != last:
                diff: int = weight - last
                return (discs[name].weight - diff, True)
    return sum(weights.values()) + disc.weight, False


def find_unbalance(discs: Dict[str, Disc]) -> int:
    needed, _ = delve(find_top_node(discs), discs)
    return needed


def find_top_node(discs: Dict[str, Disc]) -> str:
    needed: Dict[str, bool] = {d.name: False for d in discs.values()}
    for disc in discs.values():
        for dep in disc.dependencies:
            needed[dep] = True
    for k, v in needed.items():
        if not v:
            return k
    assert False


def main() -> int:
    parser = ArgumentParser()
    parser.add_argument('file')
    args = parser.parse_args()
    with open(args.file, encoding='utf-8') as f:
        discs: Dict[str, Disc] = {}
        for lineraw in f.readlines():
            line = lineraw.strip()
            disc: Disc = Disc()
            parts = line.split(" -> ")
            if len(parts) > 1:
                disc.dependencies = parts[1].split(", ")
            props = parts[0].split(" ")
            disc.name = props[0]
            disc.weight = int(props[1][1:-1], base=10)
            discs[disc.name] = disc

        print(find_top_node(discs))
        print(find_unbalance(discs))
    return 0


if __name__ == '__main__':
    exit(main())
