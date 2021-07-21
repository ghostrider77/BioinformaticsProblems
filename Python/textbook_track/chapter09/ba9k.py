# Generate the Last-to-First Mapping of a String
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


def calc_last_to_first_mapping(transformed_string, ix):
    last_column = create_indexed_column(transformed_string)
    first_column = calc_first_column_with_index(transformed_string)
    return first_column[last_column[ix]]


def main():
    reader = sys.stdin
    transformed_text = next(reader).rstrip()
    ix = int(next(reader))
    result = calc_last_to_first_mapping(transformed_text, ix)
    print(result)


if __name__ == "__main__":
    main()
