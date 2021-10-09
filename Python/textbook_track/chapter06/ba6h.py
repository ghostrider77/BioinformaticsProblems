# Implement ColoredEdges
import re
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

    def __len__(self):
        return len(self._synteny_blocks)

    def __iter__(self):
        yield from self._synteny_blocks


class Cycle:
    def __init__(self, nodes):
        self._nodes = tuple(nodes)

    def __repr__(self):
        return f"({' '.join(map(str, self._nodes))})"

    def __getitem__(self, k):
        if k == len(self._nodes):
            return self._nodes[0]

        return self._nodes[k]


class Genome:
    def __init__(self, chromosomes):
        self._chromosomes = chromosomes

    def __repr__(self):
        return '\n'.join(map(str, self._chromosomes))

    def __iter__(self):
        yield from self._chromosomes


def read_genome(line):
    genome_parts = re.findall(r'\((.*?)\)', line)
    chromosomes = []
    for part in genome_parts:
        synteny_blocks = []
        for item in part.split():
            signed_number = SignedNumber(Sign(item[0]), int(item[1:]))
            synteny_blocks.append(signed_number)
        chromosomes.append(Chromosome(synteny_blocks))
    return Genome(chromosomes)


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


def calc_colored_edges(genome):
    edges = []
    for chromosome in genome:
        cycle = chromosome_to_cycle(chromosome)
        n = len(chromosome)
        for ix in range(1, n+1):
            edges.append((cycle[2*ix-1], cycle[2*ix]))
    return edges


def main():
    reader = sys.stdin
    genome = read_genome(next(reader))
    result = calc_colored_edges(genome)
    print(', ' .join(f'({a}, {b})' for a, b in result))


if __name__ == '__main__':
    main()
