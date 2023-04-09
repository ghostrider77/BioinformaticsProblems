# Counting Point Mutations
import sys


def calc_hamming_distance(s1, s2):
    distance = 0
    for c1, c2 in zip(s1, s2):
        if c1 != c2:
            distance += 1
    return distance


def main():
    reader = sys.stdin
    s1 = next(reader).strip()
    s2 = next(reader).strip()
    result = calc_hamming_distance(s1, s2)
    print(result)


if __name__ == '__main__':
    main()
