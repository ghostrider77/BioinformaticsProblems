# Compute the Number of Peptides of Given Total Mass

# Remark: this code is valid only if peptides are represented as integer sequences instead of amino acid sequences.
# There is only one 'peptide' with mass 113 when it is represented with its masses (113 itself),
# but there are two real peptides ('I' and 'L') that have mass 113.
import functools as ft
import sys

from textbook_track.resources.utils import read_amino_acid_mass_table


def calc_nr_peptides_with_given_mass(total_mass, mass_table):
    amino_acid_masses = frozenset(mass_table.values())

    @ft.lru_cache(maxsize=None)
    def solve(current_mass):
        if current_mass < 0:
            return 0
        if current_mass == 0:
            return 1
        return sum(solve(current_mass - mass) for mass in amino_acid_masses)

    if total_mass <= 0:
        return 0
    return solve(total_mass)


def main():
    data = sys.stdin.read().splitlines()
    total_mass = int(data[0])
    mass_table = read_amino_acid_mass_table()
    result = calc_nr_peptides_with_given_mass(total_mass, mass_table)
    print(result)


if __name__ == '__main__':
    main()
