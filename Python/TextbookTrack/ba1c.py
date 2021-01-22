# Find the Reverse Complement of a String
import sys


NUCLEOTIDE_COMPLEMENTS = {'A': 'T', 'C': 'G', 'T': 'A', 'G': 'C'}


def calc_reverse_complement(genome):
    return ''.join(map(NUCLEOTIDE_COMPLEMENTS.get, reversed(genome)))


def main():
    data = sys.stdin.read().splitlines()
    dna = data[0]
    result = calc_reverse_complement(dna)
    print(result)


if __name__ == '__main__':
    main()
