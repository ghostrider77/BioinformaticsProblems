# Find the Most Frequent Words in a String
import sys

from collections import Counter


def most_frequent_k_mers(text, k):
    counts = Counter()
    for ix in range(len(text)-k+1):
        k_mer = text[ix:ix+k]
        counts.update([k_mer])

    max_count = counts.most_common(n=1)[0][1]
    return [k_mer for k_mer, count in counts.items() if count == max_count]


def main():
    data = sys.stdin.read().splitlines()
    text = data[0]
    k = int(data[1])
    result = most_frequent_k_mers(text, k)
    print(' '.join(result))


if __name__ == '__main__':
    main()
