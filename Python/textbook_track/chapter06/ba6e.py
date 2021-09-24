# Find All Shared k-mers of a Pair of Strings
import sys

from collections import defaultdict

NUCLEOTIDE_COMPLEMENTS = {'A': 'T', 'C': 'G', 'T': 'A', 'G': 'C'}


def calc_reverse_complement(genome):
    return ''.join(map(NUCLEOTIDE_COMPLEMENTS.get, reversed(genome)))


def collect_k_mer_indices(genome, k):
    indices = defaultdict(list)
    for ix in range(len(genome)-k+1):
        k_mer1 = genome[ix:ix+k]
        indices[k_mer1].append(ix)
    return dict(indices)


def calc_shared_k_mers(s1, s2, k):
    k_mer_indices = collect_k_mer_indices(s1, k)
    shared_k_mer_indices = []
    for jy in range(len(s2)-k+1):
        k_mer = s2[jy:jy+k]
        reversed_k_mer = calc_reverse_complement(k_mer)
        indices_in_s1 = k_mer_indices.get(k_mer, []) + k_mer_indices.get(reversed_k_mer, [])
        for ix in indices_in_s1:
            shared_k_mer_indices.append((ix, jy))
    return shared_k_mer_indices


def main():
    reader = sys.stdin
    k = int(next(reader))
    s1 = next(reader).rstrip()
    s2 = next(reader).rstrip()
    result = calc_shared_k_mers(s1, s2, k)
    for x, y in result:
        print(f'({x}, {y})')


if __name__ == '__main__':
    main()
