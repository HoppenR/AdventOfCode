from argparse import ArgumentParser
from typing import List
from enum import Enum


class Token(Enum):
    BRACE = 0
    BRACE_END = 1
    GARBAGE = 2
    GARBAGE_END = 3
    IGNORE = 4


def count_indents(tokens: List[Token]) -> int:
    score: int = 0
    level: int = 0
    for grp in tokens:
        if grp == Token.BRACE:
            level += 1
            score += level
        elif grp == Token.BRACE_END:
            level -= 1
    return score


def count_garbage(groups: List[Token]) -> int:
    score: int = 0
    for grp in groups:
        if grp == Token.GARBAGE:
            score += 1
        elif grp == Token.GARBAGE_END:
            score -= 1
    return score


def parse_tokens(line: str) -> List[Token]:
    token_stack: List[Token] = list()
    for ch in line:
        if len(token_stack) > 0:
            if token_stack[-1] == Token.IGNORE:
                token_stack.pop()
                continue
            elif token_stack[-1] == Token.GARBAGE:
                if ch not in ['>', '!']:
                    token_stack.append(Token.GARBAGE)
                    continue
        if ch == '{':
            token_stack.append(Token.BRACE)
        elif ch == '}':
            token_stack.append(Token.BRACE_END)
        elif ch == '<':
            token_stack.append(Token.GARBAGE)
        elif ch == '>':
            token_stack.append(Token.GARBAGE_END)
        elif ch == '!':
            token_stack.append(Token.IGNORE)
    return token_stack


def main() -> int:
    parser = ArgumentParser()
    parser.add_argument('file')
    args = parser.parse_args()
    tokens: List[Token]
    with open(args.file, encoding='utf-8') as f:
        tokens = parse_tokens(f.readline().strip())
    print(count_indents(tokens))
    print(count_garbage(tokens))
    return 0


if __name__ == '__main__':
    exit(main())
