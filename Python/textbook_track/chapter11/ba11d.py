# Converting Peptide Vector into Peptide Problem
import sys

from textbook_track.resources.utils import read_amino_acid_mass_table


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def reverse_mass_table_mapping(mass_table):
    return {mass: amino_acid for amino_acid, mass in mass_table.items()}


def calc_prefix_masses(peptide_vector):
    prefix_masses = [0]
    for ix, item in enumerate(peptide_vector, start=1):
        if item == 1:
            prefix_masses.append(ix)
    return prefix_masses


def restore_peptide_from_peptide_vector(peptide_vector, mass_table):
    prefix_masses = calc_prefix_masses(peptide_vector)
    inverse_mapping = reverse_mass_table_mapping(mass_table)
    peptide = []
    for mass1, mass2 in zip(prefix_masses, prefix_masses[1:]):
        mass = mass2 - mass1
        if (amino_acid := inverse_mapping.get(mass)) is None:
            return None
        peptide.append(amino_acid)
    return ''.join(peptide)


def main():
    reader = sys.stdin
    peptide_vector = convert_to_intlist(next(reader))
    mass_table = read_amino_acid_mass_table()
    result = restore_peptide_from_peptide_vector(peptide_vector, mass_table)
    print(result)


if __name__ == '__main__':
    main()
