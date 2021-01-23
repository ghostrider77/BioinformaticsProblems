# Implement PatternToNumber
import sys

NUCLEOTIDE_ORDER = {'A': 0, 'C': 1, 'G': 2, 'T': 3}


def pattern_to_number(pattern):
    encoding = 0
    for nucleotide in pattern:
        encoding = 4 * encoding + NUCLEOTIDE_ORDER[nucleotide]
    return encoding


def main():
    data = sys.stdin.read().splitlines()
    dna = data[0]
    result = pattern_to_number(dna)
    print(result)


if __name__ == '__main__':
    main()
