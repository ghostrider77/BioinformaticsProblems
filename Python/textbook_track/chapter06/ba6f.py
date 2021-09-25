# Implement ChromosomeToCycle
import sys

from enum import Enum


class Sign(Enum):
    PLUS = '+'
    MINUS = '-'


class SignedNumber:
    def __init__(self, sign, number):
        self._sign = sign
        self._number = number

    def __repr__(self):
        return f'{self._sign.value}{self._number}'

    @property
    def sign(self):
        return self._sign

    @property
    def number(self):
        return self._number


class Chromosome:
    def __init__(self, synteny_blocks):
        self._synteny_blocks = tuple(synteny_blocks)

    def __repr__(self):
        return f"({' '.join(map(str, self._synteny_blocks))})"

    def __iter__(self):
        yield from self._synteny_blocks


class Cycle:
    def __init__(self, nodes):
        self._nodes = tuple(nodes)

    def __repr__(self):
        return f"({' '.join(map(str, self._nodes))})"


def read_chromosome(line):
    line = line.replace('(', '').replace(')', '')
    synteny_blocks = []
    for item in line.split():
        signed_number = SignedNumber(Sign(item[0]), int(item[1:]))
        synteny_blocks.append(signed_number)
    return Chromosome(synteny_blocks)


def chromosome_to_cycle(chromosome):
    nodes = []
    for block in chromosome:
        if block.sign == Sign.PLUS:
            nodes.append(2*block.number-1)
            nodes.append(2*block.number)
        else:
            nodes.append(2*block.number)
            nodes.append(2*block.number-1)
    return Cycle(nodes)


def main():
    reader = sys.stdin
    chromosome = read_chromosome(next(reader))
    result = chromosome_to_cycle(chromosome)
    print(result)


if __name__ == '__main__':
    main()
