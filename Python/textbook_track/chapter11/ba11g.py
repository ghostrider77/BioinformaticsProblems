# Implement PSMSearch
import itertools as it
import math
import sys

from textbook_track.resources.utils import read_amino_acid_mass_table


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def calc_peptide_prefix_masses(peptide, mass_table):
    return list(it.accumulate(peptide, lambda acc, amino_acid: acc + mass_table[amino_acid], initial=0))


def calc_peptide_score(peptide, spectrum, total_mass, mass_table):
    prefix_masses = calc_peptide_prefix_masses(peptide, mass_table)
    if prefix_masses[-1] != total_mass:
        return -math.inf

    return sum(spectrum[ix-1] for ix in prefix_masses[1:])


def find_highest_scoring_peptide_in_proteome(spectrum, proteome, mass_table):
    mass_values = frozenset(mass_table.values())
    total_mass = len(spectrum)
    min_peptide_length = math.ceil(total_mass / max(mass_values))
    max_peptide_length = total_mass // min(mass_values)
    best_peptide = ''
    best_score = -math.inf
    for k in range(min_peptide_length, max_peptide_length+1):
        for ix in range(len(proteome)-k+1):
            peptide = proteome[ix:ix+k]
            score = calc_peptide_score(peptide, spectrum, total_mass, mass_table)
            if score > best_score:
                best_peptide = peptide
                best_score = score
    return best_peptide, best_score


def find_peptide_spectrum_match_pairs(spectrum_vectors, proteome, threshold, mass_table):
    peptides = set()
    for spectrum in spectrum_vectors:
        peptide, score = find_highest_scoring_peptide_in_proteome(spectrum, proteome, mass_table)
        if score >= threshold:
            peptides.add(peptide)
    return peptides


def main():
    data = sys.stdin.read().splitlines()
    spectrum_vectors = [convert_to_intlist(line) for line in data[:-2]]
    proteome = data[-2]
    threshold = int(data[-1])
    mass_table = read_amino_acid_mass_table()
    result = find_peptide_spectrum_match_pairs(spectrum_vectors, proteome, threshold, mass_table)
    for peptide in result:
        print(peptide)


if __name__ == '__main__':
    main()
