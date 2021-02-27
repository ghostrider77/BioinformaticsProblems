# Find the Length of a Longest Path in a Manhattan-like Grid
import sys


def convert_to_intlist(line):
    return [int(item) for item in line.split()]


def read_weight_matrix(reader, nr_rows):
    matrix = []
    for _ in range(nr_rows):
        row = convert_to_intlist(next(reader))
        matrix.append(row)
    return matrix


def calc_longest_path(n, m, down_weights, right_weights):
    longest_path = [[0] * (m + 1) for _ in range(n+1)]
    for ix in range(1, n+1):
        longest_path[ix][0] = longest_path[ix-1][0] + down_weights[ix-1][0]
    for jy in range(1, m+1):
        longest_path[0][jy] = longest_path[0][jy-1] + right_weights[0][jy-1]

    for ix in range(1, n+1):
        for jy in range(1, m+1):
            path_down = longest_path[ix-1][jy] + down_weights[ix-1][jy]
            path_right = longest_path[ix][jy-1] + right_weights[ix][jy-1]
            longest_path[ix][jy] = max(path_down, path_right)
    return longest_path[n][m]


def main():
    reader = sys.stdin
    n, m = convert_to_intlist(next(reader))
    down = read_weight_matrix(reader, n)
    _ = next(reader)
    right = read_weight_matrix(reader, n+1)
    result = calc_longest_path(n, m, down, right)
    print(result)


if __name__ == '__main__':
    main()
