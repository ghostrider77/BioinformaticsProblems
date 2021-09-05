# Find a Highest-Scoring Modified Peptide against a Spectrum
import math
import sys

from textbook_track.resources.utils import read_amino_acid_mass_table


class Matrix:
    def __init__(self, n, m, layers, *, initial_value):
        self._layers = layers
        self._matrix = [[[initial_value] * layers for _ in range(m)] for _ in range(n)]

    def __call__(self, ixs):
        return self[ixs]

    def __getitem__(self, ixs):
        i, j, k = ixs
        return self._matrix[i][j][k]

    def __setitem__(self, ixs, v):
        i, j, k = ixs
        self._matrix[i][j][k] = v

    @property
    def nr_layers(self):
        return self._layers


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def get_predecessors_of_a_node(ix, jx, t, diff):
    predecessors = []
    if jx - diff[ix-1] >= 0:
        predecessors.append((ix - 1, jx - diff[ix-1], t))

    for jx_prime in range(jx):
        predecessors.append((ix - 1, jx_prime, t - 1))

    return predecessors


def argmax(score_matrix):
    scores = (score_matrix[-1, -1, t] for t in range(score_matrix.nr_layers))
    max_index, _ = max(enumerate(scores), key=lambda x: x[1])
    return max_index


def calc_highest_scoring_peptide_with_modification(backtrack, maximal_index, peptide, diff):
    n = len(peptide)
    current_index = maximal_index
    modified_peptide = []
    while n > 0:
        previous_index = backtrack[current_index]
        modification = current_index[1] - previous_index[1] - diff[n-1]
        if modification == 0:
            modified_peptide.append(peptide[n-1])
        elif modification > 0:
            modified_peptide.append(f'{peptide[n-1]}(+{modification})')
        else:
            modified_peptide.append(f'{peptide[n-1]}({modification})')
        current_index = previous_index
        n -= 1

    return ''.join(modified_peptide[::-1])


def solve_spectral_alignment_problem(peptide, spectrum, mass_table, k):
    diff = tuple(map(mass_table.get, peptide))
    nr_rows = len(diff) + 1
    nr_cols = len(spectrum) + 1
    nr_layers = k + 1

    score = Matrix(nr_rows, nr_cols, nr_layers, initial_value=-math.inf)
    backtrack = {}

    score[0, 0, 0] = 0
    for ix in range(1, nr_rows):
        for jx in range(1, nr_cols):
            if jx - diff[ix-1] >= 0:
                score[ix, jx, 0] = spectrum[jx-1] + score[ix-1, jx-diff[ix-1], 0]
                backtrack[(ix, jx, 0)] = (ix-1, jx-diff[ix-1], 0)

    for ix in range(1, nr_rows):
        for jx in range(1, nr_cols):
            for t in range(1, nr_layers):
                predecessors = get_predecessors_of_a_node(ix, jx, t, diff)
                predecessor_scores = tuple(map(score, predecessors))
                max_index, max_score = max(zip(predecessors, predecessor_scores), key=lambda x: x[1])
                score[ix, jx, t] = spectrum[jx-1] + max_score
                backtrack[(ix, jx, t)] = max_index

    maximal_index = (nr_rows-1, nr_cols - 1, argmax(score))
    return calc_highest_scoring_peptide_with_modification(backtrack, maximal_index, peptide, diff)


def main():
    reader = sys.stdin
    peptide = next(reader).rstrip()
    spectrum = convert_to_intlist(next(reader))
    k = int(next(reader))
    mass_table = read_amino_acid_mass_table()
    result = solve_spectral_alignment_problem(peptide, spectrum, mass_table, k)
    print(result)


if __name__ == '__main__':
    main()
