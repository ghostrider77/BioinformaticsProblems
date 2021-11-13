# Binary Search
import sys


def convert_to_intlist(line):
    return tuple(int(item) for item in line.split())


def find_elems_in_sorted_array(array, queries, n):
    def binary_search(item, left, right):
        if left > right:
            return -1

        middle_ix = (left + right) // 2
        middle_elem = array[middle_ix]
        if middle_elem == item:
            return middle_ix + 1

        if middle_elem < item:
            return binary_search(item, middle_ix + 1, right)

        return binary_search(item, left, middle_ix - 1)

    return (binary_search(k, 0, n - 1) for k in queries)


def main():
    reader = sys.stdin
    n = int(next(reader))
    _ = next(reader)
    array = convert_to_intlist(next(reader))
    queries = convert_to_intlist(next(reader))
    result = find_elems_in_sorted_array(array, queries, n)
    print(' '.join(map(str, result)))


if __name__ == '__main__':
    main()
