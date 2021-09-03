# Sequence a Peptide
import sys

from collections import defaultdict, namedtuple
from math import inf

from textbook_track.resources.utils import read_amino_acid_mass_table

Node = namedtuple('Node', ['node_id', 'weight'])


class SpectralGraph:
    def __init__(self, spectral_vector, inverse_mass_table):
        self._nodes = self._create_nodes(spectral_vector)
        self._adjacency_list = self._build_spectrum_graph(inverse_mass_table)
        self._incoming_edges = self._calc_incoming_edges()

    @property
    def nodes(self):
        return self._nodes

    @property
    def source(self):
        return self._nodes[0]

    @property
    def sink(self):
        return self._nodes[-1]

    def predecessors(self, node):
        return self._incoming_edges.get(node, [])

    def _create_nodes(self, spectral_vector):
        nodes = [Node(node_id=0, weight=0)]
        for ix, s in enumerate(spectral_vector, start=1):
            nodes.append(Node(node_id=ix, weight=s))
        return tuple(nodes)

    def _build_spectrum_graph(self, inverse_mass_table):
        adjacency_list = defaultdict(list)
        for ix, node1 in enumerate(self._nodes):
            for node2 in self._nodes[ix+1:]:
                mass = node2.node_id - node1.node_id
                if (label := inverse_mass_table.get(mass)) is not None:
                    adjacency_list[node1].append((node2, label))
        return dict(adjacency_list)

    def _calc_incoming_edges(self):
        ingraph = defaultdict(list)
        for node, neighbours in self._adjacency_list.items():
            for neighbour, _ in neighbours:
                ingraph[neighbour].append(node)
        return dict(ingraph)


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def reverse_mass_table_mapping(mass_table):
    return {mass: amino_acid for amino_acid, mass in mass_table.items()}


def collect_longest_path(backtrack, source, sink):
    path = [sink]
    node = sink
    while node != source:
        node = backtrack[node]
        path.append(node)
    return path[::-1]


def find_longest_path(graph):
    longest_path = {}
    backtrack = {}
    for node in graph.nodes:
        predecessors = graph.predecessors(node)
        if node == graph.source:
            longest_path[node] = 0
        elif predecessors:
            paths_from_predecessors = ((u, longest_path.get(u, -inf) + node.weight) for u in predecessors)
            v, s = max(paths_from_predecessors, key=lambda x: x[1])
            longest_path[node] = s
            backtrack[node] = v

    return collect_longest_path(backtrack, graph.source, graph.sink)


def restore_peptide_from_path(path, inverse_mass_table):
    peptide = []
    for node1, node2 in zip(path, path[1:]):
        mass = node2.node_id - node1.node_id
        amino_acid = inverse_mass_table[mass]
        peptide.append(amino_acid)
    return ''.join(peptide)


def run_peptide_sequencing(spectral_vector, mass_table):
    inverse_mass_table = reverse_mass_table_mapping(mass_table)
    graph = SpectralGraph(spectral_vector, inverse_mass_table)
    path = find_longest_path(graph)
    return restore_peptide_from_path(path, inverse_mass_table)


def main():
    reader = sys.stdin
    spectral_vector = convert_to_intlist(next(reader))
    mass_table = read_amino_acid_mass_table()
    result = run_peptide_sequencing(spectral_vector, mass_table)
    print(result)


if __name__ == '__main__':
    main()
