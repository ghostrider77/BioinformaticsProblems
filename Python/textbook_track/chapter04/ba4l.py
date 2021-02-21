# Trim a Peptide Leaderboard
import itertools as it
import operator
import sys

from collections import Counter

from textbook_track.resources.utils import read_amino_acid_mass_table


class Peptide:
    def __init__(self, peptide_string, mass_table):
        self._peptide = peptide_string
        self._masses = [mass_table[amino_acid] for amino_acid in peptide_string]
        self._total_mass = sum(self._masses)

    def __str__(self):
        return self._peptide

    def __len__(self):
        return len(self._masses)

    @property
    def masses(self):
        return tuple(self._masses)


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def calc_peptide_prefix_masses(peptide):
    return list(it.accumulate(peptide.masses, operator.add, initial=0))


def calc_theoretical_linear_spectrum(peptide):
    prefix_masses = calc_peptide_prefix_masses(peptide)
    n = len(peptide)
    spectrum = [0]
    for start_ix in range(n):
        for end_ix in range(start_ix+1, n+1):
            subpeptide_mass = prefix_masses[end_ix] - prefix_masses[start_ix]
            spectrum.append(subpeptide_mass)
    return Counter(spectrum)


def calc_consistency_score(peptide, experimental_spectrum, spectrum_calculator):
    spectrum = spectrum_calculator(peptide)
    return sum(min(experimental_spectrum.get(mass, 0), count) for mass, count in spectrum.items())


def trim_leaderboard(leaderboard, experimental_spectrum, limit):
    if not leaderboard:
        return leaderboard

    scored_peptides = []
    for peptide in leaderboard:
        score = calc_consistency_score(peptide, experimental_spectrum, calc_theoretical_linear_spectrum)
        scored_peptides.append((peptide, score))

    sorted_peptides = sorted(scored_peptides, key=lambda x: x[1], reverse=True)
    trimmed_board = sorted_peptides[:limit]
    tie_score = trimmed_board[-1][1]
    return [peptide for peptide, score in sorted_peptides if score >= tie_score]


def read_peptides(peptide_strings, mass_table):
    peptides = []
    for peptide in peptide_strings.split():
        peptides.append(Peptide(peptide, mass_table))
    return peptides


def main():
    data = sys.stdin.read().splitlines()
    experimental_spectrum = Counter(convert_to_intlist(data[1]))
    limit = int(data[2])
    mass_table = read_amino_acid_mass_table()
    leaderboard = read_peptides(data[0], mass_table)
    result = trim_leaderboard(leaderboard, experimental_spectrum, limit)
    print(' '.join(map(str, result)))


if __name__ == '__main__':
    main()
