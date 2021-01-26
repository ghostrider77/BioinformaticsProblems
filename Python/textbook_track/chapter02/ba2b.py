# Find a Median String
import itertools as it
import sys

NUCLEOTIDES = ('A', 'C', 'G', 'T')


def calc_hamming_distance(s1, s2):
    distance = 0
    for c1, c2 in zip(s1, s2):
        if c1 != c2:
            distance += 1
    return distance


def distance_between_pattern_and_text(text, pattern, k):
    minimal_distance = k
    for ix in range(len(text)-k+1):
        k_mer = text[ix:ix+k]
        distance = calc_hamming_distance(pattern, k_mer)
        if distance < minimal_distance:
            minimal_distance = distance
    return minimal_distance


def distance_between_pattern_and_collection_of_texts(texts, pattern, k):
    distance = 0
    for text in texts:
        distance += distance_between_pattern_and_text(text, pattern, k)
    return distance


def calc_median_string(texts, k):
    nr_texts = len(texts)
    minimum_distance = nr_texts * k
    median_string = ''
    for pattern in map(''.join, it.product(NUCLEOTIDES, repeat=k)):
        distance = distance_between_pattern_and_collection_of_texts(texts, pattern, k)
        if distance < minimum_distance:
            minimum_distance = distance
            median_string = pattern
    return median_string


def main():
    data = sys.stdin.read().splitlines()
    k = int(data[0])
    texts = data[1:]
    result = calc_median_string(texts, k)
    print(result)


if __name__ == '__main__':
    main()
