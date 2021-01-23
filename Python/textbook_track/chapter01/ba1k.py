# Generate the Frequency Array of a String
import sys

NUCLEOTIDE_ORDER = {'A': 0, 'C': 1, 'G': 2, 'T': 3}


def pattern_to_number(pattern):
    encoding = 0
    for nucleotide in pattern:
        encoding = 4 * encoding + NUCLEOTIDE_ORDER[nucleotide]
    return encoding


def computing_frequencies(text, k):
    frequencies = [0] * 4 ** k
    for ix in range(len(text)-k+1):
        pattern = text[ix:ix+k]
        code = pattern_to_number(pattern)
        frequencies[code] += 1
    return frequencies


def main():
    data = sys.stdin.read().splitlines()
    dna = data[0]
    k = int(data[1])
    result = computing_frequencies(dna, k)
    print(' '.join(map(str, result)))


if __name__ == '__main__':
    main()
