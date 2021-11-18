# Building a Heap
import sys


def convert_to_intlist(line):
    return [int(item) for item in line.split()]


def get_index_of_parent_children_maximum(array, parent_ix, size):
    max_ix = parent_ix
    left_child_ix = 2*parent_ix + 1
    if left_child_ix < size and array[left_child_ix] > array[max_ix]:
        max_ix = left_child_ix

    right_child_ix = left_child_ix + 1
    if right_child_ix < size and array[right_child_ix] > array[max_ix]:
        max_ix = right_child_ix

    return max_ix


def sift_down(array, parent_ix, size):
    while (max_ix := get_index_of_parent_children_maximum(array, parent_ix, size)) != parent_ix:
        array[max_ix], array[parent_ix] = array[parent_ix], array[max_ix]
        parent_ix = max_ix


def heapify(array, size):
    parent_ix = size // 2 - 1
    while parent_ix >= 0:
        sift_down(array, parent_ix, size)
        parent_ix -= 1


def main():
    reader = sys.stdin
    n = int(next(reader))
    array = convert_to_intlist(next(reader))
    heapify(array, n)
    print(' '.join(map(str, array)))


if __name__ == '__main__':
    main()
