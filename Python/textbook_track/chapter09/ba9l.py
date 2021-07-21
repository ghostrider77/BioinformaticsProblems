# Implement BWMatching
import functools as ft
import sys

from collections import defaultdict


def create_indexed_column(string):
    counts = defaultdict(int)
    char_index = []
    for letter in string:
        count = counts[letter]
        char_index.append((letter, count))
        counts[letter] += 1
    return char_index


def calc_first_column_with_index(transformed_string):
    first_column = create_indexed_column(sorted(transformed_string))
    return {char_index: ix for ix, char_index in enumerate(first_column)}


def find_pattern_in_text(pattern, first_column, last_column):
    top_pointer = 0
    bottom_pointer = len(last_column) - 1
    while pattern:
        letter = pattern.pop()
        letter_occurrence_indices = []
        for ix, (char, _) in enumerate(last_column[top_pointer:bottom_pointer+1], start=top_pointer):
            if char == letter:
                letter_occurrence_indices.append(ix)

        if not letter_occurrence_indices:
            return 0

        top_letter_index = min(letter_occurrence_indices)
        bottom_letter_index = max(letter_occurrence_indices)
        top_pointer = first_column[last_column[top_letter_index]]
        bottom_pointer = first_column[last_column[bottom_letter_index]]

    return bottom_pointer - top_pointer + 1


def perform_pattern_matching(transformed_text, patterns):
    last_column = create_indexed_column(transformed_text)
    first_column = calc_first_column_with_index(transformed_text)
    count_matches = ft.partial(find_pattern_in_text, first_column=first_column, last_column=last_column)
    return [count_matches(list(pattern)) for pattern in patterns]


def main():
    reader = sys.stdin
    transformed_text = next(reader).rstrip()
    patterns = next(reader).split()
    result = perform_pattern_matching(transformed_text, patterns)
    print(' '.join(map(str, result)))


if __name__ == "__main__":
    main()
