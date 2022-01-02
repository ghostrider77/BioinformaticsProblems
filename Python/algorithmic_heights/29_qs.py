# Quick Sort
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


def quicksort(array, n):
    stack = [(0, n - 1)]
    while stack:
        start_ix, end_ix = stack.pop(-1)
        if start_ix < end_ix:
            random_ix = random.randint(start_ix, end_ix)
            pivot = array[random_ix]
            middle_start, middle_end = three_way_partitioning(array, pivot, start_ix, end_ix)
            stack.append((start_ix, middle_start - 1))
            stack.append((middle_end, end_ix))


def main():
    reader = sys.stdin
    n = int(next(reader))
    array = convert_to_intlist(next(reader))
    quicksort(array, n)
    print(' '.join(map(str, array)))


if __name__ == '__main__':
    main()
