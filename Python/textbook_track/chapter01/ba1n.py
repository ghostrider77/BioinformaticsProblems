# Generate the d-Neighborhood of a String
import sys

NUCLEOTIDES = ('A', 'C', 'G', 'T')


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


def main():
    data = sys.stdin.read().splitlines()
    pattern = data[0]
    d = int(data[1])
    result = generate_neighbourhood(pattern, d)
    for string in result:
        print(string)


if __name__ == '__main__':
    main()
