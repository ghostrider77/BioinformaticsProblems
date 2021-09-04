# Compute the Size of a Spectral Dictionary
import functools as ft
import sys

from textbook_track.resources.utils import read_amino_acid_mass_table


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def calc_spectral_dictionary_size(spectrum, mass_table, threshold, max_score):
    amino_acid_masses = tuple(mass_table.values())
    m = len(spectrum)

    @ft.lru_cache(maxsize=None)
    def solve(ix, jy):
        if ix == 0 and jy == 0:
            return 1
        if ix <= 0:
            return 0
        return sum(solve(ix-mass, jy-spectrum[ix-1]) for mass in amino_acid_masses)

    return sum(solve(m, score) for score in range(threshold, max_score+1))


def main():
    reader = sys.stdin
    spectrum = convert_to_intlist(next(reader))
    threshold = int(next(reader))
    max_score = int(next(reader))
    mass_table = read_amino_acid_mass_table()
    result = calc_spectral_dictionary_size(spectrum, mass_table, threshold, max_score)
    print(result)


if __name__ == '__main__':
    main()
