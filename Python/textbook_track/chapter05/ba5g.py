# Compute the Edit Distance Between Two Strings
import sys


def calc_edit_distance(string1, string2):
    n = len(string1)
    m = len(string2)
    shortest_path = [[0] * (m+1) for _ in range(n+1)]
    for ix in range(1, n+1):
        shortest_path[ix][0] = ix
    for jy in range(1, m+1):
        shortest_path[0][jy] = jy

    for ix, c1 in enumerate(string1):
        for jy, c2 in enumerate(string2):
            deletion = shortest_path[ix][jy+1] + 1
            insertion = shortest_path[ix+1][jy] + 1
            mismatch = shortest_path[ix][jy] + int(c1 != c2)
            shortest_path[ix+1][jy+1] = min(insertion, deletion, mismatch)
    return shortest_path[n][m]


def main():
    data = sys.stdin.read().splitlines()
    string1 = data[0]
    string2 = data[1]
    result = calc_edit_distance(string1, string2)
    print(result)


if __name__ == '__main__':
    main()
