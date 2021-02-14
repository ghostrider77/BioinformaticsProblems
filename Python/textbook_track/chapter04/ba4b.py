# Find Substrings of a Genome Encoding a Given Amino Acid String
import sys

from textbook_track.resources.utils import read_genetic_code

NUCLEOTIDE_COMPLEMENTS = {'A': 'T', 'C': 'G', 'T': 'A', 'G': 'C'}


def calc_reverse_complement(genome):
    return ''.join(map(NUCLEOTIDE_COMPLEMENTS.get, reversed(genome)))


def generate_codons(rna):
    for ix in range(len(rna) // 3):
        yield rna[3*ix:3*(ix+1)]


def transcript_dna(dna):
    return ''.join(('U' if nucleotide == 'T' else nucleotide for nucleotide in dna))


def does_k_mer_translate_to_peptide(k_mer, peptide, genetic_code):
    codons = generate_codons(k_mer)
    return all(genetic_code.get(codon) == amino_acid for codon, amino_acid in zip(codons, peptide))


def find_peptide_encoding_substrings(dna, peptide, genetic_code):
    length = len(dna)
    forward_rna = transcript_dna(dna)
    reverse_rna = transcript_dna(calc_reverse_complement(dna))
    k = 3 * len(peptide)
    encoding_substrings = []
    for ix in range(length-k+1):
        pattern = forward_rna[ix:ix+k]
        reverse_pattern = reverse_rna[length-ix-k:length-ix]
        if (does_k_mer_translate_to_peptide(pattern, peptide, genetic_code) or
            does_k_mer_translate_to_peptide(reverse_pattern, peptide, genetic_code)):
            encoding_substrings.append(dna[ix:ix+k])
    return encoding_substrings


def main():
    data = sys.stdin.read().splitlines()
    dna = data[0]
    peptide = data[1]
    genetic_code = read_genetic_code()
    result = find_peptide_encoding_substrings(dna, peptide, genetic_code)
    for substring in result:
        print(substring)


if __name__ == '__main__':
    main()
