# Implement BetterBWMatching
import functools as ft
import sys

from collections import Counter


def calc_first_occurrences(transformed_string):
    letter_counts = Counter(transformed_string)
    first_occurrence = {}
    ix = 0
    for letter in sorted(letter_counts.keys()):
        first_occurrence[letter] = ix
        ix += letter_counts[letter]
    return first_occurrence


def calc_count_matrix(transformed_string, unique_characters):
    length = len(transformed_string)
    count_matrix = {letter: [0] * (length+1) for letter in unique_characters}
    for ix, current_letter in enumerate(transformed_string):
        for char in unique_characters:
            counts = count_matrix[char]
            counts[ix+1] = counts[ix] + 1 if current_letter == char else counts[ix]
    return count_matrix


def find_pattern_in_text(pattern, first_occurrences, count_matrix, length):
    top_pointer = 0
    bottom_pointer = length - 1
    while pattern:
        letter = pattern.pop()
        if letter in first_occurrences:
            top_pointer = first_occurrences[letter] + count_matrix[letter][top_pointer]
            bottom_pointer = first_occurrences[letter] + count_matrix[letter][bottom_pointer+1] - 1
        else:
            return 0

        if top_pointer > bottom_pointer:
            return 0

    return bottom_pointer - top_pointer + 1


def perform_pattern_matching(transformed_text, patterns):
    length = len(transformed_text)
    first_occurrences = calc_first_occurrences(transformed_text)
    unique_letters = frozenset(first_occurrences.keys())
    count_matrix = calc_count_matrix(transformed_text, unique_letters)
    count_matches = ft.partial(find_pattern_in_text, first_occurrences=first_occurrences, count_matrix=count_matrix,
                               length=length)
    return [count_matches(list(pattern)) for pattern in patterns]


def main():
    reader = sys.stdin
    transformed_text = next(reader).rstrip()
    patterns = next(reader).split()
    result = perform_pattern_matching(transformed_text, patterns)
    print(' '.join(map(str, result)))


if __name__ == "__main__":
    main()
