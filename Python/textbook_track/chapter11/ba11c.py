# Convert a Peptide into a Peptide Vector
import itertools as it
import sys

from textbook_track.resources.utils import read_amino_acid_mass_table


def calc_peptide_prefix_masses(peptide, mass_table):
    return list(it.accumulate(peptide, lambda acc, amino_acid: acc + mass_table[amino_acid], initial=0))


def calc_peptide_vector(peptide, mass_table):
    prefix_masses = calc_peptide_prefix_masses(peptide, mass_table)
    total_mass = prefix_masses[-1]
    peptide_vector = [0] * total_mass
    for mass in prefix_masses[1:]:
        peptide_vector[mass-1] = 1
    return peptide_vector


def main():
    reader = sys.stdin
    peptide = next(reader).rstrip()
    mass_table = read_amino_acid_mass_table()
    result = calc_peptide_vector(peptide, mass_table)
    print(' '.join(map(str, result)))


if __name__ == '__main__':
    main()
