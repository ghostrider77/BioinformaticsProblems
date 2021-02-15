from pathlib import Path


def read_genetic_code():
    file_path = Path(__file__).with_name('RNA_codon_table.txt')
    with open(file_path, 'r') as f:
        code = {}
        for line in f:
            try:
                codon, amino_acid = line.split()
                code[codon] = amino_acid
            except ValueError:
                pass
    return code


def read_amino_acid_mass_table():
    file_path = Path(__file__).with_name('integer_mass_table.txt')
    with open(file_path, 'r') as f:
        mass_table = {}
        for line in f:
            amino_acid, mass = line.split()
            mass_table[amino_acid] = int(mass)
    return mass_table
