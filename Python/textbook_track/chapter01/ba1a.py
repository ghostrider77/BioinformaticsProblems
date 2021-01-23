# Compute the Number of Times a Pattern Appears in a Text
import sys


def count_pattern(text, pattern):
    pattern_length = len(pattern)
    count = 0
    for ix in range(len(text)-pattern_length+1):
        substring = text[ix:ix+pattern_length]
        if substring == pattern:
            count += 1
    return count


def main():
    text, pattern = sys.stdin.read().splitlines()
    result = count_pattern(text, pattern)
    print(result)


if __name__ == '__main__':
    main()
