from argparse import ArgumentParser

numbers: dict[str, int] = {
    'zero': 0, 'one': 1, 'two': 2, 'three': 3, 'four': 4,
    'five': 5, 'six': 6, 'seven': 7, 'eight': 8, 'nine': 9,
}


def find_num(x: str, r: range, extnum: bool) -> tuple[int, int]:
    for ix in r:
        if extnum:
            for num, val in numbers.items():
                if x[ix:ix+len(num)] == num:
                    return ix, val
        if '0' <= x[ix] <= '9':
            return ix, int(x[ix])
    assert False, 'unreachable'


def sum_calibration_values(lines: list[str], extnum: bool) -> int:
    sum: int = 0
    for line in lines:
        f_ix, f_num = find_num(line, range(0, len(line)-1), extnum)
        l_ix, l_num = find_num(line, range(len(line)-1, f_ix-1, -1), extnum)
        sum += f_num * 10 + l_num
    return sum


def main() -> int:
    parser = ArgumentParser()
    parser.add_argument('file')
    args = parser.parse_args()
    with open(args.file, encoding='utf-8') as f:
        lines: list[str] = f.readlines()
        print(sum_calibration_values(lines, False))
        print(sum_calibration_values(lines, True))
    return 0


if __name__ == '__main__':
    exit(main())
