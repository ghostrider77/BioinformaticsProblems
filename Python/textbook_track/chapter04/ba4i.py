# Implement ConvolutionCyclopeptideSequencing
import itertools as it
import operator
import sys

from collections import Counter, defaultdict
from copy import deepcopy


class Peptide:
    def __init__(self, masses):
        self._masses = masses
        self._total_mass = sum(masses)

    def __str__(self):
        return '-'.join(map(str, self._masses))

    def __len__(self):
        return len(self._masses)

    @property
    def masses(self):
        return tuple(self._masses)

    @property
    def total_mass(self):
        return self._total_mass

    def update(self, mass):
        self._masses.append(mass)
        self._total_mass += mass


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def calc_peptide_prefix_masses(peptide):
    return list(it.accumulate(peptide.masses, operator.add, initial=0))


def expand(peptides, amino_acid_masses):
    for peptide in peptides:
        for mass in amino_acid_masses:
            new_peptide = deepcopy(peptide)
            new_peptide.update(mass)
            yield new_peptide


def calc_theoretical_linear_spectrum(peptide):
    prefix_masses = calc_peptide_prefix_masses(peptide)
    n = len(peptide)
    spectrum = [0]
    for start_ix in range(n):
        for end_ix in range(start_ix+1, n+1):
            subpeptide_mass = prefix_masses[end_ix] - prefix_masses[start_ix]
            spectrum.append(subpeptide_mass)
    return Counter(spectrum)


def calc_theoretical_cyclic_spectrum(peptide):
    prefix_masses = calc_peptide_prefix_masses(peptide)
    peptide_mass = prefix_masses[-1]
    n = len(peptide)
    cyclospectrum = [0]
    for start_ix in range(n):
        for end_ix in range(start_ix+1, n+1):
            subpeptide_mass = prefix_masses[end_ix] - prefix_masses[start_ix]
            cyclospectrum.append(subpeptide_mass)
            if start_ix > 0 and end_ix < n:
                cyclospectrum.append(peptide_mass - subpeptide_mass)
    return Counter(cyclospectrum)


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


def calc_convolution_of_spectrum(spectrum, m):
    convolution = defaultdict(int)
    for mass1, count1 in spectrum.items():
        for mass2, count2 in spectrum.items():
            difference = mass1 - mass2
            if 57 <= difference <= 200:
                convolution[difference] += count1 * count2

    sorted_counts = sorted(convolution.items(), key=lambda x: x[1], reverse=True)
    trimmed_counts = sorted_counts[:m]
    tie_count = trimmed_counts[-1][1]
    return frozenset({mass for mass, count in convolution.items() if count >= tie_count})


def convolution_cyclopeptide_sequencing(experimental_spectrum, limit, m):
    parent_mass = max(experimental_spectrum)
    experimental_spectrum = Counter(experimental_spectrum)
    amino_acid_masses = calc_convolution_of_spectrum(experimental_spectrum, m)
    leader_peptide = Peptide([])
    leader_score = 0
    leaderboard = [leader_peptide]
    while leaderboard:
        candidate_peptides = expand(leaderboard, amino_acid_masses)
        leaderboard = []
        for peptide in candidate_peptides:
            if peptide.total_mass == parent_mass:
                score = calc_consistency_score(peptide, experimental_spectrum, calc_theoretical_cyclic_spectrum)
                if score > leader_score:
                    leader_peptide = peptide
                    leader_score = score
            elif peptide.total_mass < parent_mass:
                leaderboard.append(peptide)
        leaderboard = trim_leaderboard(leaderboard, experimental_spectrum, limit)
    return leader_peptide


def main():
    data = sys.stdin.read().splitlines()
    m = int(data[0])
    limit = int(data[1])
    experimental_spectrum = convert_to_intlist(data[2])
    result = convolution_cyclopeptide_sequencing(experimental_spectrum, limit, m)
    print(result)


if __name__ == '__main__':
    main()
