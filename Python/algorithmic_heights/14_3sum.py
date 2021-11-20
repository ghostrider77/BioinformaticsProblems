# 3SUM
import sys


def convert_to_intlist(line):
    return tuple([int(item) for item in line.split()])


def read_arrays(reader, k):
    return [convert_to_intlist(next(reader)) for _ in range(k)]


def solve_2_sum(array, target, target_ix):
    negative_target_indices = {}
    for ix, item in enumerate(array[target_ix+1:], start=target_ix+1):
        if (jy := negative_target_indices.get(item)) is not None:
            return sorted((jy+1, ix+1, target_ix+1))

        negative_target_indices[target-item] = ix
    return None


def find_zero_sum_index_pairs(arrays):
    def solve_3sum(array):
        for ix, item in enumerate(array):
            if (indices := solve_2_sum(array, -item, ix)) is not None:
                return indices
        return (-1,)

    return [solve_3sum(array) for array in arrays]


def main():
    reader = sys.stdin
    k, _ = convert_to_intlist(next(reader))
    arrays = read_arrays(reader, k)
    result = find_zero_sum_index_pairs(arrays)
    for indices in result:
        print(' '.join(map(str, indices)))


if __name__ == '__main__':
    main()
