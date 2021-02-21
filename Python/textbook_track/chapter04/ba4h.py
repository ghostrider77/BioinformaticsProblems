# Generate the Convolution of a Spectrum
import itertools as it
import sys

from collections import Counter, defaultdict


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def sort_convolution(convolution):
    sorted_convolution = sorted(convolution.items(), key=lambda x: x[1], reverse=True)
    return list(it.chain.from_iterable(map(lambda mass_counts: it.repeat(*mass_counts), sorted_convolution)))


def calc_convolution_of_spectrum(spectrum):
    convolution = defaultdict(int)
    for mass1, count1 in spectrum.items():
        for mass2, count2 in spectrum.items():
            difference = mass1 - mass2
            if difference > 0:
                convolution[difference] += count1 * count2
    return sort_convolution(dict(convolution))


def main():
    data = sys.stdin.read().splitlines()
    spectrum = Counter(convert_to_intlist(data[0]))
    result = calc_convolution_of_spectrum(spectrum)
    print(' '.join(map(str, result)))


if __name__ == '__main__':
    main()
