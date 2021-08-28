# Construct the Graph of a Spectrum
import sys

from collections import defaultdict

from textbook_track.resources.utils import read_amino_acid_mass_table


class Graph:
    def __init__(self, spectrum, mass_table):
        self._spectrum = self._verify_spectrum(spectrum)
        self._adjacency_list = self._build_spectrum_graph(mass_table)

    def edges(self):
        for node, neighbours in self._adjacency_list.items():
            for neighbour, label in neighbours:
                yield f'{node}->{neighbour}:{label}'

    @staticmethod
    def _verify_spectrum(spectrum):
        if (zero_mass := 0) not in spectrum:
            spectrum.append(zero_mass)
        return sorted(spectrum)

    def _build_spectrum_graph(self, mass_table):
        inverse_mapping = reverse_mass_table_mapping(mass_table)
        adjacency_list = defaultdict(list)
        for ix, node1 in enumerate(self._spectrum):
            for node2 in self._spectrum[ix+1:]:
                weight_difference = node2 - node1
                if (label := inverse_mapping.get(weight_difference)) is not None:
                    adjacency_list[node1].append((node2, label))
        return dict(adjacency_list)


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def reverse_mass_table_mapping(mass_table):
    return {mass: amino_acid for amino_acid, mass in mass_table.items()}


def main():
    reader = sys.stdin
    spectrum = convert_to_intlist(next(reader))
    mass_table = read_amino_acid_mass_table()
    graph = Graph(spectrum, mass_table)
    for edge in graph.edges():
        print(edge)


if __name__ == '__main__':
    main()
