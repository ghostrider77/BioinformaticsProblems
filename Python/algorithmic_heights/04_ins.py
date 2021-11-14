# Insertion Sort
import sys


def convert_to_intlist(line):
    return [int(item) for item in line.split()]


def calc_nr_swaps_in_insertion_sort(array, n):
    nr_swaps = 0
    for ix in range(1, n):
        k = ix
        while k > 0 and array[k] < array[k-1]:
            array[k], array[k-1] = array[k-1], array[k]
            nr_swaps += 1
            k -= 1
    return nr_swaps


def main():
    reader = sys.stdin
    n = int(next(reader))
    array = convert_to_intlist(next(reader))
    result = calc_nr_swaps_in_insertion_sort(array, n)
    print(result)


if __name__ == '__main__':
    main()
