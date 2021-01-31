# Implement DistanceBetweenPatternAndStrings
import sys


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


def main():
    data = sys.stdin.read().splitlines()
    pattern = data[0]
    k = len(pattern)
    texts = data[1].split()
    result = distance_between_pattern_and_collection_of_texts(texts, pattern, k)
    print(result)


if __name__ == '__main__':
    main()
