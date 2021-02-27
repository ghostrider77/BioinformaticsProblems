# Find a Longest Common Subsequence of Two Strings
import sys


def collect_longest_common_subsequence(backtrack, string, ix, jy):
    lcs = []
    while ix > 0 and jy > 0:
        if backtrack[ix-1][jy-1] == 0:
            lcs.append(string[ix-1])
            ix -= 1
            jy -= 1
        elif backtrack[ix-1][jy-1] == -1:
            ix -= 1
        else:
            jy -= 1
    return ''.join(lcs[::-1])


def calc_longest_common_subsequence(string1, string2):
    n1 = len(string1)
    n2 = len(string2)
    table = [[0] * (n2+1) for _ in range(n1+1)]
    backtrack = [[0] * n2 for _ in range(n1)]

    for ix, c1 in enumerate(string1):
        for jy , c2 in enumerate(string2):
            path_down = table[ix][jy+1]
            path_right = table[ix+1][jy]
            path_diag = table[ix][jy] + int(c1 == c2)
            max_path = max(path_down, path_right, path_diag)
            table[ix+1][jy+1] = max_path

            if max_path == path_down:
                backtrack[ix][jy] = -1
            elif max_path == path_right:
                backtrack[ix][jy] = 1

    return collect_longest_common_subsequence(backtrack, string1, n1, n2)


def main():
    data = sys.stdin.read().splitlines()
    string1 = data[0]
    string2 = data[1]
    result = calc_longest_common_subsequence(string1, string2)
    print(result)


if __name__ == '__main__':
    main()
