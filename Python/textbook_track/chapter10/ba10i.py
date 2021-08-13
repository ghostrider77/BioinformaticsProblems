# Implement Viterbi Learning
import math
import sys

from collections import namedtuple

EPSILON = 1e-10
DIGITS = 3

HMM = namedtuple('HMM', ['alphabet', 'states', 'transition', 'emission'])


class Label:
    def __init__(self, labels):
        self._labels = labels
        self._label_indices = {item: ix for ix, item in enumerate(labels)}
        self._size = len(labels)

    def __getitem__(self, label):
        return self._label_indices[label]

    def __repr__(self):
        return ', '.join(map(str, self._labels))

    def __iter__(self):
        yield from self._labels

    @property
    def labels(self):
        return self._labels

    @property
    def size(self):
        return self._size


class ProbabilityMatrix:
    def __init__(self, row_labels, column_labels, probabilities=None):
        self._row_labels = row_labels
        self._column_labels = column_labels
        self._probabilities = self._initialize_matrix(probabilities)

    def __getitem__(self, label_pair):
        row_label, col_label = label_pair
        return self._probabilities[self._row_labels[row_label]][self._column_labels[col_label]]

    def __setitem__(self, label_pair, value):
        row_label, col_label = label_pair
        self._probabilities[self._row_labels[row_label]][self._column_labels[col_label]] = value

    def __repr__(self):
        lines = ['\t' + '\t'.join(self._column_labels)]
        for label in self._row_labels:
            probs = self._probabilities[self._row_labels[label]]
            row = '\t'.join(str(round(p, DIGITS)) for p in probs)
            lines.append(f'{label}\t{row}')
        return '\n'.join(lines)

    def _initialize_matrix(self, probabilities):
        if probabilities is not None:
            return probabilities

        return [[EPSILON] * self.nr_cols for _ in range(self.nr_rows)]

    def rowsum(self, label):
        return sum(self._probabilities[self._row_labels[label]])

    @property
    def nr_rows(self):
        return self._row_labels.size

    @property
    def nr_cols(self):
        return self._column_labels.size


def read_matrix(reader, k):
    _ = next(reader)
    matrix = []
    for _ in range(k):
        line = next(reader)
        probabilities = [float(item) for item in line.split()[1:]]
        matrix.append(probabilities)
    return matrix


def read_data(reader):
    n = int(next(reader))
    _ = next(reader)
    string = next(reader).rstrip()
    _ = next(reader)
    alphabet = Label(next(reader).split())
    _ = next(reader)
    states = Label(next(reader).split())
    _ = next(reader)
    transition = ProbabilityMatrix(states, states, read_matrix(reader, states.size))
    _ = next(reader)
    emission = ProbabilityMatrix(states, alphabet, read_matrix(reader, states.size))
    hmm = HMM(alphabet=alphabet, states=states, transition=transition, emission=emission)
    return string, hmm, n


def calc_argmax(sequence):
    return max(enumerate(sequence), key=lambda x: x[1])


def estimate_transition_probabilities(hidden_path, states):
    transition = ProbabilityMatrix(states, states)
    for state_from, state_to in zip(hidden_path, hidden_path[1:]):
        transition[state_from, state_to] += 1

    for s1 in states:
        rowsum = transition.rowsum(s1)
        for s2 in states:
            transition[s1, s2] /= rowsum
    return transition


def estimate_emission_probabilities(emitted_string, hidden_path, alphabet, states):
    emission = ProbabilityMatrix(states, alphabet)
    for state, char in zip(hidden_path, emitted_string):
        emission[state, char] += 1

    for state in states:
        rowsum = emission.rowsum(state)
        for letter in alphabet:
            emission[state, letter] /= rowsum
    return emission


def estimate_hmm_parameters(emitted_string, hidden_path, alphabet, states):
    transition = estimate_transition_probabilities(hidden_path, states)
    emission = estimate_emission_probabilities(emitted_string, hidden_path, alphabet, states)
    return HMM(alphabet, states, transition, emission)


def reconstruct_optimal_hidden_path(backtrack, nr_cols, maximal_score_index, states):
    hidden_path = [states.labels[maximal_score_index]]
    jy = nr_cols
    while jy > 0:
        jy -= 1
        maximal_score_index = backtrack[maximal_score_index][jy]
        hidden_path.append(states.labels[maximal_score_index])
    return ''.join(hidden_path[::-1])


def calc_most_likely_hidden_path(hmm, emitted_string):
    _, states, transition, emission = hmm
    nr_cols = len(emitted_string)
    logscore_matrix = [[0.0] * nr_cols for _ in range(states.size)]
    backtrack = [[0] * (nr_cols - 1) for _ in range(states.size)]
    char = emitted_string[0]
    for ix, state in enumerate(states):
        logscore_matrix[ix][0] = math.log(1 / states.size) + math.log(emission[state, char])

    for jy, char in enumerate(emitted_string[1:], start=1):
        for ix, state in enumerate(states):
            emission_logprob = math.log(emission[state, char])
            logscores = [logscore_matrix[k][jy-1] + math.log(transition[previous_state, state]) + emission_logprob
                         for k, previous_state in enumerate(states)]
            index, maximum_sum_weight = calc_argmax(logscores)
            logscore_matrix[ix][jy] = maximum_sum_weight
            backtrack[ix][jy-1] = index
    maximal_score_index, _ = calc_argmax(map(lambda x: x[-1], logscore_matrix))
    return reconstruct_optimal_hidden_path(backtrack, nr_cols-1, maximal_score_index, states)


def run_Viterbi_learning(hmm, emitted_string, n):
    for _ in range(n):
        hidden_path = calc_most_likely_hidden_path(hmm, emitted_string)
        hmm = estimate_hmm_parameters(emitted_string, hidden_path, hmm.alphabet, hmm.states)
    return hmm


def main():
    reader = sys.stdin
    emitted_string, hmm, n = read_data(reader)
    result = run_Viterbi_learning(hmm, emitted_string, n)
    print(result.transition)
    print('--------')
    print(result.emission)


if __name__ == '__main__':
    main()
