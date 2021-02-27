# Find a Highest-Scoring Alignment of Two Strings
import sys

from textbook_track.resources.utils import read_scoring_matrix


def collect_global_alignment(backtrack, string1, string2):
    ix = len(string1)
    jy = len(string2)
    aligned = []
    while ix > 0 or jy > 0:
        if ix == 0:
            aligned.append(('-', string2[jy-1]))
            jy -= 1
        elif jy == 0:
            aligned.append((string1[ix-1], '-'))
            ix -= 1
        elif backtrack[ix-1][jy-1] == 0:
            aligned.append((string1[ix-1], string2[jy-1]))
            ix -= 1
            jy -= 1
        elif backtrack[ix-1][jy-1] == -1:
            aligned.append((string1[ix-1], '-'))
            ix -= 1
        else:
            aligned.append(('-', string2[jy-1]))
            jy -= 1

    return map(''.join, zip(*aligned[::-1]))


def calc_global_alignment(string1, string2, scoring_matrix, sigma):
    n = len(string1)
    m = len(string2)
    longest_path = [[0] * (m+1) for _ in range(n+1)]
    backtrack = [[0] * m for _ in range(n)]
    for ix in range(1, n+1):
        longest_path[ix][0] = -ix * sigma
    for jy in range(1, m+1):
        longest_path[0][jy] = -jy * sigma

    for ix, c1 in enumerate(string1):
        for jy, c2 in enumerate(string2):
            deletion = longest_path[ix][jy+1] - sigma
            insertion = longest_path[ix+1][jy] - sigma
            match = longest_path[ix][jy] + scoring_matrix[(c1, c2)]
            longest_path[ix+1][jy+1] = max(insertion, deletion, match)

            if longest_path[ix+1][jy+1] == deletion:
                backtrack[ix][jy] = -1
            elif longest_path[ix+1][jy+1] == insertion:
                backtrack[ix][jy] = 1

    aligned_string1, aligned_string2 = collect_global_alignment(backtrack, string1, string2)
    return longest_path[n][m], aligned_string1, aligned_string2


def main():
    data = sys.stdin.read().splitlines()
    string1 = data[0]
    string2 = data[1]
    blosum62 = read_scoring_matrix()
    sigma = 5
    score, aligned_string1, aligned_string2 = calc_global_alignment(string1, string2, blosum62, sigma)
    print(score)
    print(aligned_string1)
    print(aligned_string2)


if __name__ == '__main__':
    main()
