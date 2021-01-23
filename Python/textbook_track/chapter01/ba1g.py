# Compute the Hamming Distance Between Two Strings
import sys


def calc_hamming_distance(s1, s2):
    distance = 0
    for c1, c2 in zip(s1, s2):
        if c1 != c2:
            distance += 1
    return distance


def main():
    data = sys.stdin.read().splitlines()
    dna1 = data[0]
    dna2 = data[1]
    result = calc_hamming_distance(dna1, dna2)
    print(result)


if __name__ == '__main__':
    main()
