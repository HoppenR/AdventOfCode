from argparse import ArgumentParser
from typing import List


def password_is_valid(password: List[str], rule: int) -> bool:
    seen: List[str] = []
    for word in password:
        if rule == 2:
            word = ''.join(sorted(word))
        if word not in seen:
            seen.append(word)
        else:
            return False
    return True

def count_valid_passwords(passwords: List[List[str]], rule: int) -> int:
    valid: int = 0
    for p in passwords:
        if password_is_valid(p, rule):
            valid += 1
    return valid

def main() -> int:
    parser = ArgumentParser()
    parser.add_argument('file')
    args = parser.parse_args()
    with open(args.file, encoding='utf-8') as f:
        passwords: List[List[str]] = [password.strip().split(' ') for password in f.readlines()]
        print(count_valid_passwords(passwords, 1))
        print(count_valid_passwords(passwords, 2))
    return 0

if __name__ == '__main__':
    exit(main())
