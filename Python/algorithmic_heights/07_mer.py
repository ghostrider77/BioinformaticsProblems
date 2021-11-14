# Merge Two Sorted Arrays
import sys


def convert_to_intlist(line):
    return tuple(int(item) for item in line.split())


def merge_sorted_arrays(array_1, n, array_2, m):
    merged_list = []
    ix = 0
    jy = 0
    while ix < n or jy < m:
        if ix == n and jy < m:
            merged_list.append(array_2[jy])
            jy += 1
        elif ix < n and jy == m:
            merged_list.append(array_1[ix])
            ix += 1
        elif array_1[ix] <= array_2[jy]:
            merged_list.append(array_1[ix])
            ix += 1
        else:
            merged_list.append(array_2[jy])
            jy += 1
    return merged_list


def main():
    reader = sys.stdin
    n = int(next(reader))
    array_1 = convert_to_intlist(next(reader))
    m = int(next(reader))
    array_2 = convert_to_intlist(next(reader))
    result = merge_sorted_arrays(array_1, n, array_2, m)
    print(' '.join(map(str, result)))


if __name__ == '__main__':
    main()
