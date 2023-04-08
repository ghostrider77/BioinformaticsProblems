# Complementing a Strand of DNA
import sys

NUCLEOTIDE_COMPLEMENTS = {'A': 'T', 'C': 'G', 'T': 'A', 'G': 'C'}


def calc_reverse_complement(dna):
    return ''.join(NUCLEOTIDE_COMPLEMENTS[nucleotide] for nucleotide in reversed(dna))


def main():
    reader = sys.stdin
    dna = next(reader).strip()
    result = calc_reverse_complement(dna)
    print(result)


if __name__ == '__main__':
    main()
