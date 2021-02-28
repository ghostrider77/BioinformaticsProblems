# Find a Highest-Scoring Alignment of Two Strings
import sys

from textbook_track.resources.utils import read_scoring_matrix


def collect_local_alignment(backtrack, string1, string2, max_row_ix, max_col_ix):
    ix = max_row_ix
    jy = max_col_ix
    aligned = []
    while ix > 0 or jy > 0:
        if ix == 0:
            aligned.append(('-', string2[jy-1]))
            jy -= 1
        elif jy == 0:
            aligned.append((string1[ix-1], '-'))
            ix -= 1
        elif backtrack[ix-1][jy-1] == 0:
            aligned.append((string1[ix-1], '-'))
            ix -= 1
        elif backtrack[ix-1][jy-1] == 1:
            aligned.append(('-', string2[jy-1]))
            jy -= 1
        elif backtrack[ix-1][jy-1] == 2:
            aligned.append((string1[ix-1], string2[jy-1]))
            ix -= 1
            jy -= 1
        else:
            ix = 0
            jy = 0

    return map(''.join, zip(*aligned[::-1]))


def initialize_tables(n, m):
    longest_path = [[0] * (m+1) for _ in range(n+1)]
    backtrack = [[0] * m for _ in range(n)]
    return longest_path, backtrack


def calc_maximum_in_table(longest_path):
    max_value = 0
    max_row_ix, max_col_ix = 0, 0
    for ix, row in enumerate(longest_path):
        max_ix, max_value_in_row = max(enumerate(row), key=lambda x: x[1])
        if max_value_in_row > max_value:
            max_value = max_value_in_row
            max_row_ix = ix
            max_col_ix = max_ix
    return max_row_ix, max_col_ix, max_value


def calc_local_alignment(string1, string2, scoring_matrix, sigma):
    n = len(string1)
    m = len(string2)
    longest_path, backtrack = initialize_tables(n, m)

    for ix, c1 in enumerate(string1):
        for jy, c2 in enumerate(string2):
            deletion = longest_path[ix][jy+1] - sigma
            insertion = longest_path[ix+1][jy] - sigma
            match = longest_path[ix][jy] + scoring_matrix[(c1, c2)]
            maximum = max(deletion, insertion, match, 0)
            longest_path[ix+1][jy+1] = maximum
            if maximum == deletion:
                backtrack[ix][jy] = 0
            elif maximum == insertion:
                backtrack[ix][jy] = 1
            elif maximum == match:
                backtrack[ix][jy] = 2
            else:
                backtrack[ix][jy] = 3

    max_row_ix, max_col_ix, max_value = calc_maximum_in_table(longest_path)
    longest_path[n][m] = max_value
    aligned_string1, aligned_string2 = collect_local_alignment(backtrack, string1, string2, max_row_ix, max_col_ix)
    return longest_path[n][m], aligned_string1, aligned_string2


def main():
    data = sys.stdin.read().splitlines()
    string1 = data[0]
    string2 = data[1]
    pam250 = read_scoring_matrix('PAM250')
    sigma = 5
    score, aligned_string1, aligned_string2 = calc_local_alignment(string1, string2, pam250, sigma)
    print(score)
    print(aligned_string1)
    print(aligned_string2)


if __name__ == '__main__':
    main()
