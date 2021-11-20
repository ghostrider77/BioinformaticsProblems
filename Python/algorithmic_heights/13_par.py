# 2-Way Partition
import sys


def convert_to_intlist(line):
    return [int(item) for item in line.split()]


def two_way_partitioning(array, pivot, n):
    start_ix = 1
    end_ix = n - 1
    while start_ix <= end_ix:
        elem = array[start_ix]
        if elem <= pivot:
            array[start_ix], array[start_ix-1] = array[start_ix-1], elem
            start_ix += 1
        else:
            array[start_ix], array[end_ix] = array[end_ix], elem
            end_ix -= 1


def main():
    reader = sys.stdin
    n = int(next(reader))
    array = convert_to_intlist(next(reader))
    two_way_partitioning(array, array[0], n)
    print(' '.join(map(str, array)))


if __name__ == '__main__':
    main()
