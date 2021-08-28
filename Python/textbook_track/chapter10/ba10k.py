# Baum-Welch Learning Problem
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


def estimate_transition_probabilities(states, emitted_string, edge_responsibilities):
    transition = ProbabilityMatrix(states, states)
    for state_l in states:
        for state_k in states:
            column = (edge_responsibilities[(state_l, state_k, ix)] for ix, _ in enumerate(emitted_string[:-1]))
            transition[state_l, state_k] = sum(column)

    for s1 in states:
        rowsum = transition.rowsum(s1)
        for s2 in states:
            transition[s1, s2] /= rowsum

    return transition


def estimate_emission_probabilities(states, alphabet, emitted_string, node_responsibilities):
    emission = ProbabilityMatrix(states, alphabet)
    for state in states:
        for letter in alphabet:
            emission_probabilities = (node_responsibilities[(state, ix)]
                                      for ix, char in enumerate(emitted_string) if char == letter)
            emission[state, letter] = sum(emission_probabilities)

    for state in states:
        rowsum = emission.rowsum(state)
        for letter in alphabet:
            emission[state, letter] /= rowsum
    return emission


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


def calc_node_responsibilities(forward_matrix, backward_matrix, states, emitted_string):
    forward_sink = sum(map(lambda x: x[-1], forward_matrix))
    sink_logprobability = math.log(forward_sink)
    responsibilities = {}
    for jy, _ in enumerate(emitted_string):
        for ix, state in enumerate(states):
            log_p = math.log(forward_matrix[ix][jy]) + math.log(backward_matrix[ix][jy]) - sink_logprobability
            responsibilities[(state, jy)] = math.exp(log_p)
    return responsibilities


def calc_edge_responsibilities(forward_matrix, backward_matrix, hmm, emitted_string):
    _, states, transition, emission = hmm
    forward_sink = sum(map(lambda x: x[-1], forward_matrix))
    sink_logprobability = math.log(forward_sink)
    responsibilities = {}
    for ix, next_char in enumerate(emitted_string[1:]):
        for kx, state_k in enumerate(states):
            for lx, state_l in enumerate(states):
                log_edge_weight_l_k = math.log(transition[state_l, state_k]) + math.log(emission[state_k, next_char])
                log_p = (math.log(forward_matrix[lx][ix]) + math.log(backward_matrix[kx][ix+1]) +
                         log_edge_weight_l_k - sink_logprobability)
                responsibilities[(state_l, state_k, ix)] = math.exp(log_p)
    return responsibilities


def expectation_step(hmm, emitted_string):
    forward_matrix = run_forward_algorithm(hmm, emitted_string)
    backward_matrix = run_backward_algorithm(hmm, emitted_string)
    node_responsibilities = calc_node_responsibilities(forward_matrix, backward_matrix, hmm.states, emitted_string)
    edge_responsibilities = calc_edge_responsibilities(forward_matrix, backward_matrix, hmm, emitted_string)
    return node_responsibilities, edge_responsibilities


def maximization_step(hmm, emitted_string, node_responsibilities, edge_responsibilities):
    alphabet, states, _, _ = hmm
    transition = estimate_transition_probabilities(states, emitted_string, edge_responsibilities)
    emission = estimate_emission_probabilities(states, alphabet, emitted_string, node_responsibilities)
    return HMM(alphabet, states, transition, emission)


def run_Baum_Welch_learning(hmm, emitted_string, n):
    for _ in range(n):
        node_responsibilities, edge_responsibilities = expectation_step(hmm, emitted_string)
        hmm = maximization_step(hmm, emitted_string, node_responsibilities, edge_responsibilities)
    return hmm


def main():
    reader = sys.stdin
    emitted_string, hmm, n = read_data(reader)
    result = run_Baum_Welch_learning(hmm, emitted_string, n)
    print(result.transition)
    print('--------')
    print(result.emission)


if __name__ == '__main__':
    main()
