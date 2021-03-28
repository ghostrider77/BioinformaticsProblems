# Compute Limb Lengths in a Tree
import sys

from math import inf


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def read_distance_matrix(reader, nr_leaves):
    matrix = []
    for _ in range(nr_leaves):
        row = convert_to_intlist(next(reader))
        matrix.append(row)
    return matrix


def calc_limb_length(distances, leaf_j, nr_leaves):
    limb_length = inf
    other_leaves = [leaf for leaf in range(nr_leaves) if leaf != leaf_j]
    for leaf_i in other_leaves:
        for leaf_k in other_leaves:
            candidate = (distances[leaf_i][leaf_j] + distances[leaf_j][leaf_k] - distances[leaf_i][leaf_k]) // 2
            if candidate < limb_length:
                limb_length = candidate
    return limb_length


def main():
    reader = sys.stdin
    nr_leaves = int(next(reader))
    leaf = int(next(reader))
    distances = read_distance_matrix(reader, nr_leaves)
    result = calc_limb_length(distances, leaf, nr_leaves)
    print(result)


if __name__ == '__main__':
    main()
