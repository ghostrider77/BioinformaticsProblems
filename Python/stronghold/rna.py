# Transcribing DNA into RNA
import sys


def transcribe_dna(dna):
    return ''.join('U' if nucleotide == 'T' else nucleotide for nucleotide in dna)


def main():
    reader = sys.stdin
    dna = next(reader).strip()
    result = transcribe_dna(dna)
    print(result)


if __name__ == '__main__':
    main()
