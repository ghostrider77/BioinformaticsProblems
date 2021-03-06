# Find a Highest-Scoring Fitting Alignment of Two Strings
import sys


def collect_fitting_alignment(backtrack, string1, string2, max_row_ix):
    ix = max_row_ix
    jy = len(string2)
    aligned = []
    while ix > 0 or jy > 0:
        if ix == 0:
            aligned.append(('-', string2[jy-1]))
            jy -= 1
        elif jy == 0:
            ix = 0
        elif backtrack[ix-1][jy-1] == 0:
            aligned.append((string1[ix-1], '-'))
            ix -= 1
        elif backtrack[ix-1][jy-1] == 1:
            aligned.append(('-', string2[jy-1]))
            jy -= 1
        else:
            aligned.append((string1[ix-1], string2[jy-1]))
            ix -= 1
            jy -= 1

    return map(''.join, zip(*aligned[::-1]))


def calc_fitting_alignment(string1, string2):
    n = len(string1)
    m = len(string2)
    longest_path = [[0] * (m+1) for _ in range(n+1)]
    backtrack = [[0] * m for _ in range(n)]
    for jy in range(1, m+1):
        longest_path[0][jy] = -jy

    for ix, c1 in enumerate(string1):
        for jy, c2 in enumerate(string2):
            deletion = longest_path[ix][jy+1] - 1
            insertion = longest_path[ix+1][jy] - 1
            match = longest_path[ix][jy] + (1 if c1 == c2 else -1)
            maximum = max(deletion, insertion, match)
            longest_path[ix+1][jy+1] = maximum
            if maximum == deletion:
                backtrack[ix][jy] = 0
            elif maximum == insertion:
                backtrack[ix][jy] = 1
            else:
                backtrack[ix][jy] = 2

    max_row_ix, max_value = max(enumerate(map(lambda row: row[-1], longest_path)), key=lambda x: x[1])
    longest_path[n][m] = max_value
    aligned_string1, aligned_string2 = collect_fitting_alignment(backtrack, string1, string2, max_row_ix)
    return longest_path[n][m], aligned_string1, aligned_string2


def main():
    data = sys.stdin.read().splitlines()
    string1 = data[0]
    string2 = data[1]
    score, aligned_string1, aligned_string2 = calc_fitting_alignment(string1, string2)
    print(score)
    print(aligned_string1)
    print(aligned_string2)


if __name__ == '__main__':
    main()
