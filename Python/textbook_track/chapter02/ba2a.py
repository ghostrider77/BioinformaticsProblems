# Implement MotifEnumeration
import sys

NUCLEOTIDES = ('A', 'C', 'G', 'T')


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def calc_hamming_distance(s1, s2):
    distance = 0
    for c1, c2 in zip(s1, s2):
        if c1 != c2:
            distance += 1
    return distance


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


def does_k_mer_occurs_approximately_in_text(text, k_mer, k, d):
    for ix in range(len(text)-k+1):
        substring = text[ix:ix+k]
        if calc_hamming_distance(substring, k_mer) <= d:
            return True
    return False


def motif_enumeration(texts, k, d):
    def all_texts_approximately_contain_pattern(k_mer):
        return all(does_k_mer_occurs_approximately_in_text(text, k_mer, k, d) for text in texts)

    motifs = set()
    for text in texts:
        for ix in range(len(text)-k+1):
            pattern = text[ix:ix+k]
            neighbours = generate_neighbourhood(pattern, d)
            for neighbour in neighbours:
                if all_texts_approximately_contain_pattern(neighbour):
                    motifs.add(neighbour)
    return motifs


def main():
    data = sys.stdin.read().splitlines()
    k, d = convert_to_intlist(data[0])
    result = motif_enumeration(data[1:], k, d)
    print(' '.join(result))


if __name__ == '__main__':
    main()
