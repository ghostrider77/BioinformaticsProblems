# Translate an RNA String into an Amino Acid String
import itertools as it
import sys

from textbook_track.resources.utils import read_genetic_code


def generate_codons(rna):
    for ix in range(len(rna) // 3):
        yield rna[3*ix:3*(ix+1)]


def translate_rna(rna, genetic_code):
    codons = generate_codons(rna)
    amino_acids = map(genetic_code.get, codons)
    protein = it.takewhile(lambda x: x is not None, amino_acids)
    return ''.join(protein)


def main():
    data = sys.stdin.read().splitlines()
    rna = data[0]
    genetic_code = read_genetic_code()
    result = translate_rna(rna, genetic_code)
    print(result)


if __name__ == '__main__':
    main()
