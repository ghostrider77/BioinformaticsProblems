# Find the Most Frequent Words with Mismatches in a String
import itertools as it
import sys

NUCLEOTIDES = ('A', 'C', 'G', 'T')
NUCLEOTIDE_COMPLEMENTS = {'A': 'T', 'C': 'G', 'T': 'A', 'G': 'C'}
NUCLEOTIDE_ORDER = {'A': 0, 'C': 1, 'G': 2, 'T': 3}


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def pattern_to_number(pattern):
    encoding = 0
    for nucleotide in pattern:
        encoding = 4 * encoding + NUCLEOTIDE_ORDER[nucleotide]
    return encoding


def number_to_pattern(encoding, k):
    pattern = []
    for _ in range(k):
        encoding, remainder = divmod(encoding, 4)
        pattern.append(NUCLEOTIDES[remainder])
    return ''.join(pattern[::-1])


def calc_reverse_complement(genome):
    return ''.join(map(NUCLEOTIDE_COMPLEMENTS.get, reversed(genome)))


def calc_frequencies_for_approximate_occurrences(text, k, d):
    frequencies = [0] * 4 ** k
    for ix in range(len(text)-k+1):
        exact_pattern = text[ix:ix+k]
        neighbours = generate_neighbourhood(exact_pattern, d)
        reverse_neighbours = {calc_reverse_complement(pattern) for pattern in neighbours}
        for pattern in it.chain(neighbours, reverse_neighbours):
            code = pattern_to_number(pattern)
            frequencies[code] += 1
    return frequencies


def calc_immediate_neighbours(pattern):
    neighbours = set()
    for ix, letter in enumerate(pattern):
        for nucleotide in NUCLEOTIDES:
            if nucleotide != letter:
                pattern_list = list(pattern)
                pattern_list[ix] = nucleotide
                neighbours.add(''.join(pattern_list))
    return neighbours


def generate_neighbourhood(text, d):
    neighbourhood = {text}
    for _ in range(d):
        neighbours_of_neighbours = set()
        for pattern in neighbourhood:
            immediate_neighbours = calc_immediate_neighbours(pattern)
            neighbours_of_neighbours.update(immediate_neighbours)
        neighbourhood.update(neighbours_of_neighbours)
    return neighbourhood


def most_frequent_approximate_k_mers_with_reverse_complements(text, k, d):
    frequencies = calc_frequencies_for_approximate_occurrences(text, k, d)
    max_occurrence = max(frequencies)
    patterns = []
    for ix, freq in enumerate(frequencies):
        if freq == max_occurrence:
            pattern = number_to_pattern(ix, k)
            patterns.append(pattern)
    return patterns


def main():
    data = sys.stdin.read().splitlines()
    text = data[0]
    k, d = convert_to_intlist(data[1])
    result = most_frequent_approximate_k_mers_with_reverse_complements(text, k, d)
    print(' '.join(result))


if __name__ == '__main__':
    main()
