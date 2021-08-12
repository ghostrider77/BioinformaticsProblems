# Compute the Probability of a String Emitted by an HMM
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
    def labels(self):
        return self._labels

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


def calc_probability_of_emitted_string(hmm, emitted_string):
    _, states, transition, emission = hmm
    nr_cols = len(emitted_string)
    forward_matrix = [[0.0] * nr_cols for _ in range(states.size)]
    char = emitted_string[0]
    for ix, state in enumerate(states):
        forward_matrix[ix][0] = emission[state, char] / states.size

    for jy, char in enumerate(emitted_string[1:], start=1):
        for ix, state in enumerate(states):
            emission_prob = emission[state, char]
            scores = (forward_matrix[k][jy-1] * transition[previous_state, state] * emission_prob
                      for k, previous_state in enumerate(states))
            forward_matrix[ix][jy] = sum(scores)

    return sum(map(lambda x: x[-1], forward_matrix))


def main():
    reader = sys.stdin
    emitted_string, hmm = read_data(reader)
    result = calc_probability_of_emitted_string(hmm, emitted_string)
    print(result)


if __name__ == '__main__':
    main()
