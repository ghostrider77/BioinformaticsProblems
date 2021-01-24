# Find Patterns Forming Clumps in a String
import sys

NUCLEOTIDES = ('A', 'C', 'G', 'T')
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


def compute_frequencies(text, k):
    frequencies = [0] * 4 ** k
    for ix in range(len(text)-k+1):
        pattern = text[ix:ix+k]
        code = pattern_to_number(pattern)
        frequencies[code] += 1
    return frequencies


def collect_clump_forming_k_mers(clump_array, k):
    k_mers_in_clump = []
    for ix, is_clump in enumerate(clump_array):
        if is_clump:
            k_mer = number_to_pattern(ix, k)
            k_mers_in_clump.append(k_mer)
    return k_mers_in_clump


def find_clumps_in_text(genome, k, l, t):
    clump_array = [False] * 4**k

    frequencies = compute_frequencies(genome[:l], k)
    for ix, freq in enumerate(frequencies):
        if freq >= t:
            clump_array[ix] = True

    for ix in range(1, len(genome)-l+1):
        start_k_mer = genome[ix-1:ix-1+k]
        start_code = pattern_to_number(start_k_mer)
        frequencies[start_code] -= 1
        end_k_mer = genome[ix+l-k:ix+l]
        end_code = pattern_to_number(end_k_mer)
        frequencies[end_code] += 1
        if frequencies[end_code] >= t:
            clump_array[end_code] = True

    return collect_clump_forming_k_mers(clump_array, k)


def main():
    data = sys.stdin.read().splitlines()
    genome = data[0]
    k, l, t = convert_to_intlist(data[1])
    result = find_clumps_in_text(genome, k, l, t)
    print(' '.join(result))


if __name__ == '__main__':
    main()
