# Find All Approximate Occurrences of a Pattern in a String
import sys


def calc_hamming_distance(s1, s2):
    distance = 0
    for c1, c2 in zip(s1, s2):
        if c1 != c2:
            distance += 1
    return distance


def find_approximate_pattern_occurrences(text, pattern, d):
    pattern_length = len(pattern)
    matching_indices = []
    for ix in range(len(text)-pattern_length+1):
        substring = text[ix:ix+pattern_length]
        if calc_hamming_distance(substring, pattern) <= d:
            matching_indices.append(ix)
    return matching_indices


def main():
    data = sys.stdin.read().splitlines()
    pattern = data[0]
    text = data[1]
    d = int(data[2])
    result = find_approximate_pattern_occurrences(text, pattern, d)
    print(' '.join(map(str, result)))


if __name__ == '__main__':
    main()
