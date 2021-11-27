# Median
import random
import sys


def convert_to_intlist(line):
    return [int(item) for item in line.split()]


def three_way_partitioning(array, pivot, start_ix, end_ix):
    current_ix = start_ix
    while current_ix <= end_ix:
        elem = array[current_ix]
        if elem < pivot:
            if current_ix != start_ix:
                array[current_ix], array[start_ix] = array[start_ix], elem
            current_ix += 1
            start_ix += 1
        elif elem > pivot:
            array[current_ix], array[end_ix] = array[end_ix], elem
            end_ix -= 1
        else:
            current_ix += 1
    return start_ix, end_ix + 1


def find_kth_smallest_element(array, n, k):
    start_ix = 0
    end_ix = n - 1
    kth_smallest = None
    while kth_smallest is None:
        random_ix = random.randint(start_ix, end_ix)
        pivot = array[random_ix]
        middle_start, middle_end = three_way_partitioning(array, pivot, start_ix, end_ix)
        if k <= middle_start:
            end_ix = middle_start
        elif middle_start < k <= middle_end:
            kth_smallest = array[middle_start]
        else:
            start_ix = middle_end
    return kth_smallest


def main():
    reader = sys.stdin
    n = int(next(reader))
    array = convert_to_intlist(next(reader))
    k = int(next(reader))
    result = find_kth_smallest_element(array, n, k)
    print(result)


if __name__ == '__main__':
    main()
