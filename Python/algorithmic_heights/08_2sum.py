# 2SUM
import sys


def convert_to_intlist(line):
    return [int(item) for item in line.split()]


def read_arrays(reader, k):
    return [convert_to_intlist(next(reader)) for _ in range(k)]


def find_zero_sum_index_pairs(arrays):
    def solve_2sum(array):
        negative_target_indices = {}
        for ix, item in enumerate(array):
            if (jy := negative_target_indices.get(item)) is not None:
                return (jy+1, ix+1)

            negative_target_indices[-item] = ix
        return (-1,)

    return [solve_2sum(array) for array in arrays]


def main():
    reader = sys.stdin
    k, _ = convert_to_intlist(next(reader))
    arrays = read_arrays(reader, k)
    result = find_zero_sum_index_pairs(arrays)
    for indices in result:
        print(' '.join(map(str, indices)))


if __name__ == '__main__':
    main()
