# Reconstruct a String from its Burrows-Wheeler Transform
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


def calc_inverse_Burrows_Wheeler_transform(transformed_string):
    n = len(transformed_string)
    last_column = create_indexed_column(transformed_string)
    first_column = calc_first_column_with_index(transformed_string)

    position = 0
    string = ['$']
    for _ in range(n-1):
        letter_count = last_column[position]
        string.append(letter_count[0])
        position = first_column[letter_count]
    return ''.join(string[::-1])


def main():
    reader = sys.stdin
    transformed_text = next(reader).rstrip()
    result = calc_inverse_Burrows_Wheeler_transform(transformed_text)
    print(result)


if __name__ == "__main__":
    main()
