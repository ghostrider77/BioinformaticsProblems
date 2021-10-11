# Implement GraphToGenome
import re
import sys

from collections import namedtuple
from enum import Enum

Edge = namedtuple('Edge', ['a', 'b'])


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

    def __len__(self):
        return len(self._nodes)

    def __getitem__(self, k):
        if k == len(self._nodes):
            return self._nodes[0]

        return self._nodes[k]


class Genome:
    def __init__(self, chromosomes):
        self._chromosomes = tuple(chromosomes)

    def __repr__(self):
        return ''.join(map(str, self._chromosomes))


def read_genome_graph_edges(line):
    edges = []
    for item in re.findall(r'\((.*?)\)', line):
        edge_endpoints = map(int, item.split(', '))
        edges.append(Edge(*edge_endpoints))
    return edges


def cycle_to_chromosome(cycle):
    n = len(cycle)
    synteny_blocks = []
    for ix in range(n // 2):
        node1, node2 = cycle[2*ix:2*ix+2]
        signed_number = SignedNumber(Sign.PLUS, node2 // 2) if node1 < node2 else SignedNumber(Sign.MINUS, node1 // 2)
        synteny_blocks.append(signed_number)
    return Chromosome(synteny_blocks)


def identify_cycles(colored_edges):
    cycles = []
    current_cycle = []
    for edge in colored_edges:
        if not current_cycle:
            start_node = edge.a

        current_cycle.extend(edge)
        potential_last = edge.b - 1 if edge.b % 2 == 0 else edge.b + 1
        if potential_last == start_node:
            end_node = current_cycle.pop(-1)
            cycles.append(Cycle([end_node] + current_cycle))
            current_cycle = []

    return cycles


def graph_to_genome(colored_edges):
    cycles = identify_cycles(colored_edges)
    chromosomes = map(cycle_to_chromosome, cycles)
    return Genome(chromosomes)


def main():
    reader = sys.stdin
    colored_edges = read_genome_graph_edges(next(reader))
    result = graph_to_genome(colored_edges)
    print(result)


if __name__ == '__main__':
    main()
