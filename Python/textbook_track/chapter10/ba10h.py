# HMM Parameter Estimation Problem
import sys

from collections import namedtuple

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
            row = '\t'.join(str(round(p, 3)) for p in probs)
            lines.append(f'{label}\t{row}')
        return '\n'.join(lines)

    def _initialize_matrix(self, probabilities):
        if probabilities is not None:
            return probabilities

        return [[0.0] * self.nr_cols for _ in range(self.nr_rows)]

    def rowsum(self, label):
        return sum(self._probabilities[self._row_labels[label]])

    @property
    def nr_rows(self):
        return self._row_labels.size

    @property
    def nr_cols(self):
        return self._column_labels.size


def read_data(reader):
    string = next(reader).rstrip()
    _ = next(reader)
    alphabet = Label(next(reader).split())
    _ = next(reader)
    hidden_path = next(reader).rstrip()
    _ = next(reader)
    states = Label(next(reader).split())
    return string, hidden_path, alphabet, states


def estimate_transition_probabilities(hidden_path, states):
    transition = ProbabilityMatrix(states, states)
    for state_from, state_to in zip(hidden_path, hidden_path[1:]):
        transition[state_from, state_to] += 1

    for s1 in states:
        rowsum = transition.rowsum(s1)
        for s2 in states:
            if rowsum == 0:
                transition[s1, s2] = 1 / states.size
            else:
                transition[s1, s2] /= rowsum
    return transition


def estimate_emission_probabilities(emitted_string, hidden_path, alphabet, states):
    emission = ProbabilityMatrix(states, alphabet)
    for state, char in zip(hidden_path, emitted_string):
        emission[state, char] += 1

    for state in states:
        rowsum = emission.rowsum(state)
        for letter in alphabet:
            if rowsum == 0:
                emission[state, letter] = 1 / alphabet.size
            else:
                emission[state, letter] /= rowsum
    return emission


def estimate_hmm_parameters(emitted_string, hidden_path, alphabet, states):
    transition = estimate_transition_probabilities(hidden_path, states)
    emission = estimate_emission_probabilities(emitted_string, hidden_path, alphabet, states)
    return HMM(alphabet, states, transition, emission)


def main():
    reader = sys.stdin
    emitted_string, hidden_path, alphabet, states = read_data(reader)
    result = estimate_hmm_parameters(emitted_string, hidden_path, alphabet, states)
    print(result.transition)
    print('--------')
    print(result.emission)


if __name__ == '__main__':
    main()
