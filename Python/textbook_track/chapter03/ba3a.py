# Generate the k-mer Composition of a String
import sys


def calc_k_mer_composition(text, k):
    composition = []
    for ix in range(len(text)-k+1):
        k_mer = text[ix:ix+k]
        composition.append(k_mer)
    return sorted(composition)


def main():
    data = sys.stdin.read().splitlines()
    k = int(data[0])
    text = data[1]
    result = calc_k_mer_composition(text, k)
    for k_mer in result:
        print(k_mer)


if __name__ == '__main__':
    main()
