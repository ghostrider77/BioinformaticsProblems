# Implement CycleToChromosome
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


class Cycle:
    def __init__(self, nodes):
        self._nodes = tuple(nodes)

    def __repr__(self):
        return f"({' '.join(map(str, self._nodes))})"

    def __getitem__(self, k):
        return self._nodes[k]

    def __len__(self):
        return len(self._nodes)


def read_cycle(line):
    line = line.replace('(', '').replace(')', '')
    nodes = [int(item) for item in line.split()]
    return Cycle(nodes)


def cycle_to_chromosome(cycle):
    n = len(cycle)
    synteny_blocks = []
    for ix in range(n // 2):
        node1, node2 = cycle[2*ix:2*ix+2]
        signed_number = SignedNumber(Sign.PLUS, node2 // 2) if node1 < node2 else SignedNumber(Sign.MINUS, node1 // 2)
        synteny_blocks.append(signed_number)
    return Chromosome(synteny_blocks)


def main():
    reader = sys.stdin
    cycle = read_cycle(next(reader))
    result = cycle_to_chromosome(cycle)
    print(result)


if __name__ == '__main__':
    main()
