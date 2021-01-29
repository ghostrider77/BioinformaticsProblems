# Find a Profile-most Probable k-mer in a String
import sys

from collections import namedtuple

ProfileColumn = namedtuple('ProfileColumn', ['A', 'C', 'G', 'T'])


def convert_to_floatlist(line):
    return [float(elem) for elem in line.split()]


def read_profile_matrix(lines):
    rows = [convert_to_floatlist(line) for line in lines]
    return [ProfileColumn(*column) for column in zip(*rows)]


def calc_k_mer_probabilty(k_mer, profile_matrix):
    p = 1.0
    for nucleotide, column in zip(k_mer, profile_matrix):
        p *= getattr(column, nucleotide)
    return p


def profile_most_probable_k_mer(text, profile_matrix, k):
    max_probability = 0.0
    most_probable_k_mer = text[:k]
    for ix in range(len(text)-k+1):
        k_mer = text[ix:ix+k]
        p = calc_k_mer_probabilty(k_mer, profile_matrix)
        if p > max_probability:
            max_probability = p
            most_probable_k_mer = k_mer
    return most_probable_k_mer


def main():
    data = sys.stdin.read().splitlines()
    text = data[0]
    k = int(data[1])
    matrix_columns = read_profile_matrix(data[2:])
    result = profile_most_probable_k_mer(text, matrix_columns, k)
    print(result)


if __name__ == '__main__':
    main()
