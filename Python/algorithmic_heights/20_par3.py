# 3-Way Partition
import sys


def convert_to_intlist(line):
    return [int(item) for item in line.split()]


def three_way_partitioning(array, pivot, n):
    middle_ix = 0
    end_ix = n - 1
    current_ix = 1
    while current_ix <= end_ix:
        elem = array[current_ix]
        if elem < pivot:
            array[current_ix], array[middle_ix] = array[middle_ix], elem
            current_ix += 1
            middle_ix += 1
        elif elem > pivot:
            array[current_ix], array[end_ix] = array[end_ix], elem
            end_ix -= 1
        else:
            current_ix += 1


def main():
    reader = sys.stdin
    n = int(next(reader))
    array = convert_to_intlist(next(reader))
    three_way_partitioning(array, array[0], n)
    print(' '.join(map(str, array)))


if __name__ == '__main__':
    main()
