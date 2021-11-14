# Majority Element
import sys

from collections import Counter


def convert_to_intlist(line):
    return tuple(int(item) for item in line.split())


def read_arrays(reader, k):
    return [convert_to_intlist(next(reader)) for _ in range(k)]


def calc_majority_elements(arrays, n):
    def majority_element(array):
        if array:
            counter = Counter(array)
            most_common, count = counter.most_common(n=1)[0]
            if count > n / 2:
                return most_common
        return -1

    return [majority_element(array) for array in arrays]


def main():
    reader = sys.stdin
    k, n = convert_to_intlist(next(reader))
    arrays = read_arrays(reader, k)
    result = calc_majority_elements(arrays, n)
    print(' '.join(map(str, result)))


if __name__ == '__main__':
    main()
