# Generate the Theoretical Spectrum of a Linear Peptide
import itertools as it
import sys

from textbook_track.resources.utils import read_amino_acid_mass_table


def calc_peptide_prefix_masses(peptide, mass_table):
    return list(it.accumulate(peptide, lambda mass, amino_acid: mass + mass_table[amino_acid], initial=0))


def calc_theoretical_spectrum(linear_peptide, mass_table):
    prefix_masses = calc_peptide_prefix_masses(linear_peptide, mass_table)
    n = len(linear_peptide)
    spectrum = [0]
    for start_ix in range(n):
        for end_ix in range(start_ix+1, n+1):
            subpeptide_mass = prefix_masses[end_ix] - prefix_masses[start_ix]
            spectrum.append(subpeptide_mass)
    return sorted(spectrum)


def main():
    data = sys.stdin.read().splitlines()
    peptide = data[0]
    mass_table = read_amino_acid_mass_table()
    result = calc_theoretical_spectrum(peptide, mass_table)
    print(result)


if __name__ == '__main__':
    main()
