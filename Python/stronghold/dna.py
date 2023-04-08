# Counting DNA Nucleotides
import sys

from collections import Counter

NUCLEOTIDES = ('A', 'C', 'G', 'T')


def count_nucleotides(dna):
    counter = Counter(dna)
    return [counter.get(nucleotide, 0) for nucleotide in NUCLEOTIDES]


def main():
    reader = sys.stdin
    dna = next(reader).strip()
    result = count_nucleotides(dna)
    print(' '.join(map(str, result)))


if __name__ == '__main__':
    main()
