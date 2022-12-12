from argparse import ArgumentParser
from enum import Enum
from typing import List, Dict

class OpType(Enum):
    INC = 0
    DEC = 1

class ConditionType(Enum):
    MORE = 0
    LESS = 1
    LESSEQ = 2
    MOREEQ = 3
    EQ = 4
    NEQ = 5

class Condition:
    typ: ConditionType
    lhs: str
    rhs: int
    def __init__(self, cond_str: str):
        _, lhs_str, typ_str, rhs_str = cond_str.split(" ")
        self.lhs = lhs_str
        self.rhs = int(rhs_str, 10)
        if typ_str == ">":
            self.typ = ConditionType.MORE
        elif typ_str == "<":
            self.typ = ConditionType.LESS
        elif typ_str == "<=":
            self.typ = ConditionType.LESSEQ
        elif typ_str == ">=":
            self.typ = ConditionType.MOREEQ
        elif typ_str == "==":
            self.typ = ConditionType.EQ
        elif typ_str == "!=":
            self.typ = ConditionType.NEQ
        else:
            assert(False)

    def eval(self):
        pass


class Op:
    target_register: str
    operation: OpType
    amount: int
    condition: Condition
    def __init__(self, cond, target, op, amt):
        self.condition = Condition(cond)
        self.target_register = target
        self.operation = op
        self.amount = amt

def print_ops(ops: List[Op], debug: bool) -> int:
    registers: Dict[str, int] = {}
    maxval: int = 0
    for op in ops:
        if op.condition.lhs not in registers:
            registers[op.condition.lhs] = 0

        res: bool = False
        if op.condition.typ == ConditionType.MORE:
            res = registers[op.condition.lhs] > op.condition.rhs
        elif op.condition.typ == ConditionType.LESS:
            res = registers[op.condition.lhs] < op.condition.rhs
        elif op.condition.typ == ConditionType.LESSEQ:
            res = registers[op.condition.lhs] <= op.condition.rhs
        elif op.condition.typ == ConditionType.MOREEQ:
            res = registers[op.condition.lhs] >= op.condition.rhs
        elif op.condition.typ == ConditionType.EQ:
            res = registers[op.condition.lhs] == op.condition.rhs
        elif op.condition.typ == ConditionType.NEQ:
            res = registers[op.condition.lhs] != op.condition.rhs

        if res:
            if op.target_register not in registers:
                registers[op.target_register] = 0
            if op.operation == OpType.INC:
                registers[op.target_register] += op.amount
            elif op.operation == OpType.DEC:
                registers[op.target_register] -= op.amount
            if registers[op.target_register] > maxval:
                maxval = registers[op.target_register]
    if debug:
        return maxval
    else:
        return max(registers.values())

def main() -> int:
    parser = ArgumentParser()
    parser.add_argument('file')
    args = parser.parse_args()
    ops: List[Op] = []
    with open(args.file, encoding='utf-8') as f:
        for line in f.readlines():
            op_type: OpType
            target, opstr, amount, condition = line.strip().split(" ", 3)
            if opstr == "inc":
                op_type = OpType.INC
            elif opstr == "dec":
                op_type = OpType.DEC
            else:
                assert(False)
            op: Op = Op(condition, target, op_type, int(amount))
            ops.append(op)
        print(print_ops(ops, False))
        print(print_ops(ops, True))
    return 0

if __name__ == '__main__':
    exit(main())
