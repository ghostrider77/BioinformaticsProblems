# Compute the Probability of an Outcome Given a Hidden Path
import math
import sys

from collections import namedtuple

HMM = namedtuple('HMM', ['alphabet', 'states', 'transition', 'emission'])


class Alphabet:
    def __init__(self, alphabet):
        self._alphabet_indices = create_indexed_map(alphabet)

    def __getitem__(self, letter):
        return self._alphabet_indices[letter]


class States:
    def __init__(self, states):
        self._state_indices = create_indexed_map(states)

    def __getitem__(self, state):
        return self._state_indices[state]


class Emission:
    def __init__(self, states, alphabet, probabilities):
        self._states = states
        self._alphabet = alphabet
        self._probabilities = probabilities

    def __getitem__(self, state_char):
        state, char = state_char
        return self._probabilities[self._states[state]][self._alphabet[char]]


def create_indexed_map(sequence):
    return {item: ix for ix, item in enumerate(sequence)}


def read_data(reader):
    string = next(reader).rstrip()
    _ = next(reader)
    alphabet = Alphabet(next(reader).split())
    _ = next(reader)
    hidden_path = next(reader).rstrip()
    _ = next(reader)
    states = States(next(reader).split())
    _ = next(reader)
    _ = next(reader)
    emission = []
    for line in reader:
        probabilities = [float(item) for item in line.split()[1:]]
        emission.append(probabilities)

    hmm = HMM(alphabet=alphabet, states=states, transition=None, emission=Emission(states, alphabet, emission))
    return string, hidden_path, hmm


def calc_probability_of_emitted_string_given_the_path(emitted_string, hidden_path, hmm):
    emission_matrix = hmm.emission
    log_p = 0.0
    for state, char in zip(hidden_path, emitted_string):
        log_p += math.log(emission_matrix[state, char])
    return math.exp(log_p)


def main():
    reader = sys.stdin
    emitted_string, hidden_path, hmm = read_data(reader)
    result = calc_probability_of_emitted_string_given_the_path(emitted_string, hidden_path, hmm)
    print(result)


if __name__ == '__main__':
    main()
