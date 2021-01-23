# Find All Occurrences of a Pattern in a String
import sys


def find_pattern_occurrences(text, pattern):
    pattern_length = len(pattern)
    matching_indices = []
    for ix in range(len(text)-pattern_length+1):
        substring = text[ix:ix+pattern_length]
        if substring == pattern:
            matching_indices.append(ix)
    return matching_indices


def main():
    data = sys.stdin.read().splitlines()
    pattern = data[0]
    genome = data[1]
    result = find_pattern_occurrences(genome, pattern)
    print(' '.join(map(str, result)))


if __name__ == '__main__':
    main()
