# Partial Sort
import sys


def convert_to_intlist(line):
    return [int(item) for item in line.split()]


def get_index_of_parent_children_minimum(array, parent_ix, size):
    min_ix = parent_ix
    left_child_ix = 2*parent_ix + 1
    if left_child_ix < size and array[left_child_ix] < array[min_ix]:
        min_ix = left_child_ix

    right_child_ix = left_child_ix + 1
    if right_child_ix < size and array[right_child_ix] < array[min_ix]:
        min_ix = right_child_ix

    return min_ix


def sift_down(array, parent_ix, size):
    while (min_ix := get_index_of_parent_children_minimum(array, parent_ix, size)) != parent_ix:
        array[min_ix], array[parent_ix] = array[parent_ix], array[min_ix]
        parent_ix = min_ix


def heapify(array, size):
    parent_ix = size // 2 - 1
    while parent_ix >= 0:
        sift_down(array, parent_ix, size)
        parent_ix -= 1


def partial_sort(array, size, k):
    heapify(array, size)
    n = size
    while n > size - k:
        array[0], array[n-1] = array[n-1], array[0]
        sift_down(array, 0, n-1)
        n -= 1
    return array[:-(k+1):-1]


def main():
    reader = sys.stdin
    n = int(next(reader))
    array = convert_to_intlist(next(reader))
    k = int(next(reader))
    result = partial_sort(array, n, k)
    print(' '.join(map(str, result)))


if __name__ == '__main__':
    main()
