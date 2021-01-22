# Implement NumberToPattern
import sys

NUCLEOTIDES = ('A', 'C', 'G', 'T')


def number_to_pattern(encoding, k):
    pattern = []
    for _ in range(k):
        encoding, remainder = divmod(encoding, 4)
        pattern.append(NUCLEOTIDES[remainder])
    return ''.join(pattern[::-1])



def main():
    data = sys.stdin.read().splitlines()
    encoding = int(data[0])
    k = int(data[1])
    result = number_to_pattern(encoding, k)
    print(result)


if __name__ == '__main__':
    main()
