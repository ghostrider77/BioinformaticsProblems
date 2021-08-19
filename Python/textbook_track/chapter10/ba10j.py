# Solve the Soft Decoding Problem
import math
import sys

from collections import namedtuple

DIGITS = 4

HMM = namedtuple('HMM', ['alphabet', 'states', 'transition', 'emission'])


class Label:
    def __init__(self, labels):
        self._labels = labels
        self._label_indices = {item: ix for ix, item in enumerate(labels)}
        self._size = len(labels)

    def __getitem__(self, label):
        return self._label_indices[label]

    def __repr__(self):
        return '\t'.join(self._labels)

    def __iter__(self):
        yield from self._labels

    @property
    def size(self):
        return self._size


class ProbabilityMatrix:
    def __init__(self, row_labels, column_labels, probabilities):
        self._row_labels = row_labels
        self._column_labels = column_labels
        self._probabilities = probabilities

    def __getitem__(self, label_pair):
        row_label, col_label = label_pair
        return self._probabilities[self._row_labels[row_label]][self._column_labels[col_label]]


def read_matrix(reader, k):
    _ = next(reader)
    matrix = []
    for _ in range(k):
        line = next(reader)
        probabilities = [float(item) for item in line.split()[1:]]
        matrix.append(probabilities)
    return matrix


def read_data(reader):
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
    return string, hmm


def run_forward_algorithm(hmm, emitted_string):
    _, states, transition, emission = hmm
    nr_cols = len(emitted_string)
    forward_matrix = [[0.0] * nr_cols for _ in range(states.size)]
    char = emitted_string[0]
    for ix, state in enumerate(states):
        forward_matrix[ix][0] = emission[state, char] / states.size

    for jy, char in enumerate(emitted_string[1:], start=1):
        for ix, state in enumerate(states):
            emission_probability = emission[state, char]
            scores = (forward_matrix[k][jy-1] * transition[previous_state, state] * emission_probability
                      for k, previous_state in enumerate(states))
            forward_matrix[ix][jy] = sum(scores)

    return forward_matrix


def run_backward_algorithm(hmm, emitted_string):
    _, states, transition, emission = hmm
    nr_cols = len(emitted_string)
    backward_matrix = [[0.0] * nr_cols for _ in range(states.size)]
    for ix, _ in enumerate(states):
        backward_matrix[ix][nr_cols-1] = 1.0

    for jy, char in enumerate(reversed(emitted_string[1:])):
        for ix, state in enumerate(states):
            scores = (backward_matrix[k][nr_cols-jy-1] * transition[state, next_state] * emission[next_state, char]
                      for k, next_state in enumerate(states))
            backward_matrix[ix][nr_cols-jy-2] = sum(scores)

    return backward_matrix


def solve_soft_decoding_problem(hmm, emitted_string):
    forward_matrix = run_forward_algorithm(hmm, emitted_string)
    backward_matrix = run_backward_algorithm(hmm, emitted_string)

    forward_sink = sum(map(lambda x: x[-1], forward_matrix))
    sink_logprobability = math.log(forward_sink)
    conditional_probabilites = [[0.0] * hmm.states.size for _ in emitted_string]
    for ix, _ in enumerate(emitted_string):
        for jy, _ in enumerate(hmm.states):
            log_p = math.log(forward_matrix[jy][ix]) + math.log(backward_matrix[jy][ix]) - sink_logprobability
            conditional_probabilites[ix][jy] = math.exp(log_p)
    return conditional_probabilites


def main():
    reader = sys.stdin
    emitted_string, hmm = read_data(reader)
    result = solve_soft_decoding_problem(hmm, emitted_string)
    print(hmm.states)
    for line in result:
        print('\t'.join(map(lambda x: str(round(x, DIGITS)), line)))


if __name__ == '__main__':
    main()
