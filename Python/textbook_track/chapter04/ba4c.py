# Generate the Theoretical Spectrum of a Cyclic Peptide
import itertools as it
import sys

from textbook_track.resources.utils import read_amino_acid_mass_table


def calc_peptide_prefix_masses(peptide, mass_table):
    return list(it.accumulate(peptide, lambda mass, amino_acid: mass + mass_table[amino_acid], initial=0))


def calc_theoretical_spectrum(cyclic_peptide, mass_table):
    prefix_masses = calc_peptide_prefix_masses(cyclic_peptide, mass_table)
    peptide_mass = prefix_masses[-1]
    n = len(cyclic_peptide)
    cyclospectrum = [0]
    for start_ix in range(n):
        for end_ix in range(start_ix+1, n+1):
            subpeptide_mass = prefix_masses[end_ix] - prefix_masses[start_ix]
            cyclospectrum.append(subpeptide_mass)
            if start_ix > 0 and end_ix < n:
                cyclospectrum.append(peptide_mass - subpeptide_mass)
    return sorted(cyclospectrum)


def main():
    data = sys.stdin.read().splitlines()
    cyclic_peptide = data[0]
    mass_table = read_amino_acid_mass_table()
    result = calc_theoretical_spectrum(cyclic_peptide, mass_table)
    print(result)


if __name__ == '__main__':
    main()
