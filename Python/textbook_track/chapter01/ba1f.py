# Find a Position in a Genome Minimizing the Skew
import sys


def get_skewness_argmins(genome):
    skew = 0
    min_skew_value = 0
    min_skew_indices = [0]

    for ix, nucleotide in enumerate(genome):
        if nucleotide == 'C':
            skew -= 1
        elif nucleotide == 'G':
            skew += 1

        if skew == min_skew_value:
            min_skew_indices.append(ix+1)
        elif skew < min_skew_value:
            min_skew_value = skew
            min_skew_indices = [ix+1]

    return min_skew_indices


def main():
    data = sys.stdin.read().splitlines()
    genome = data[0]
    result = get_skewness_argmins(genome)
    print(' '.join(map(str, result)))


if __name__ == '__main__':
    main()
