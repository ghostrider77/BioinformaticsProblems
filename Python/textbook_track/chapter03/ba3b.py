# Reconstruct a String from its Genome Path
import sys


def calc_string_spelled_by_a_genome_path(k_mers):
    text = list(k_mers[0])
    for k_mer in k_mers[1:]:
        text.append(k_mer[-1])
    return ''.join(text)


def main():
    data = sys.stdin.read().splitlines()
    result = calc_string_spelled_by_a_genome_path(data)
    print(result)


if __name__ == '__main__':
    main()
