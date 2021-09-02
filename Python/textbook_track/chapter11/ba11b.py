# Implement DecodingIdealSpectrum
import itertools as it
import operator
import sys

from collections import Counter, defaultdict

from textbook_track.resources.utils import read_amino_acid_mass_table


class Graph:
    def __init__(self, spectrum, inverse_mass_table):
        self._spectrum = spectrum
        self._adjacency_list = self._build_spectrum_graph(inverse_mass_table)

    def _build_spectrum_graph(self, inverse_mass_table):
        adjacency_list = defaultdict(list)
        for ix, node1 in enumerate(self._spectrum):
            for node2 in self._spectrum[ix+1:]:
                weight_difference = node2 - node1
                if (label := inverse_mass_table.get(weight_difference)) is not None:
                    adjacency_list[node1].append((node2, label))
        return dict(adjacency_list)

    def find_all_paths_to_sink(self, v, sink):
        if v == sink:
            return {(v,)}

        paths = set()
        neighbours = self._adjacency_list.get(v, [])
        for w, _ in neighbours:
            paths_from_w_to_sink = self.find_all_paths_to_sink(w, sink)
            for path in paths_from_w_to_sink:
                extended_path = (v,) + path
                paths.add(extended_path)
        return paths


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def reverse_mass_table_mapping(mass_table):
    return {mass: amino_acid for amino_acid, mass in mass_table.items()}


def verify_spectrum(spectrum):
    if (zero_mass := 0) not in spectrum:
        spectrum.append(zero_mass)
    return sorted(spectrum)


def calc_peptide_prefix_masses(peptide_masses):
    return list(it.accumulate(peptide_masses, operator.add, initial=0))


def calc_ideal_spectrum(peptide):
    prefix_masses = calc_peptide_prefix_masses(peptide)
    suffix_masses = calc_peptide_prefix_masses(peptide[::-1])
    return Counter(prefix_masses + suffix_masses[1:-1])


def calc_peptide_spelled_by_path(path):
    peptide_masses = []
    for mass1, mass2 in zip(path, path[1:]):
        mass = mass2 - mass1
        peptide_masses.append(mass)
    return peptide_masses


def find_peptide_corresponding_to_spectrum(paths, spectrum, inverse_mass_table):
    spectrum = Counter(spectrum)
    for path in paths:
        peptide_masses = calc_peptide_spelled_by_path(path)
        ideal_spectrum = calc_ideal_spectrum(peptide_masses)
        if ideal_spectrum == spectrum:
            return ''.join(map(inverse_mass_table.get, peptide_masses))
    return None


def decode_an_ideal_spectrum(spectrum, mass_table):
    spectrum = verify_spectrum(spectrum)
    inverse_mapping = reverse_mass_table_mapping(mass_table)
    graph = Graph(spectrum, inverse_mapping)
    sink = max(spectrum)
    paths = graph.find_all_paths_to_sink(0, sink)
    return find_peptide_corresponding_to_spectrum(paths, spectrum, inverse_mapping)


def main():
    reader = sys.stdin
    spectrum = convert_to_intlist(next(reader))
    mass_table = read_amino_acid_mass_table()
    result = decode_an_ideal_spectrum(spectrum, mass_table)
    print(result)


if __name__ == '__main__':
    main()
