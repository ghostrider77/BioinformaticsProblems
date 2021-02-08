# Construct a String Spelled by a Gapped Genome Path
import sys

from collections import namedtuple


class GappedPattern(namedtuple('GappedPattern', ['first', 'second'])):
    __slots__ = ()

    @property
    def prefix(self):
        return GappedPattern(self.first[:-1], self.second[:-1])

    @property
    def suffix(self):
        return GappedPattern(self.first[1:], self.second[1:])


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def read_gapped_patterns(lines):
    return [GappedPattern(*line.split('|')) for line in lines]


def calc_string_spelled_by_a_gapped_genome_path(paired_patterns, k, d):
    def calc_string_spelled_by_a_genome_path(k_mers):
        text = list(k_mers[0])
        for k_mer in k_mers[1:]:
            text.append(k_mer[-1])
        return ''.join(text)

    first_k_mers, second_kmers = zip(*paired_patterns)
    first_genome = calc_string_spelled_by_a_genome_path(first_k_mers)
    second_genome = calc_string_spelled_by_a_genome_path(second_kmers)
    if first_genome[k+d:] != second_genome[:-(k+d)]:
        return None
    return first_genome[:k+d] + second_genome


def main():
    data = sys.stdin.read().splitlines()
    k, d = convert_to_intlist(data[0])
    paired_patterns = read_gapped_patterns(data[1:])
    result = calc_string_spelled_by_a_gapped_genome_path(paired_patterns, k, d)
    print(result)


if __name__ == '__main__':
    main()
