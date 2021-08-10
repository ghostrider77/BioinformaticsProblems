# Compute the Probability of a Hidden Path
import math
import sys

from collections import namedtuple

HMM = namedtuple('HMM', ['alphabet', 'states', 'transition', 'emission'])


class States:
    def __init__(self, states):
        self._state_indices = {state: ix for ix, state in enumerate(states)}
        self._nr_states = len(states)

    def __getitem__(self, state):
        return self._state_indices[state]

    @property
    def nr_states(self):
        return self._nr_states


class Transition:
    def __init__(self, states, probabilities):
        self._states = states
        self._probabilities = probabilities

    def __getitem__(self, state_pair):
        s1, s2 = state_pair
        return self._probabilities[self._states[s1]][self._states[s2]]


def read_data(reader):
    hidden_path = next(reader).rstrip()
    _ = next(reader)
    states = States(next(reader).split())
    _ = next(reader)
    _ = next(reader)
    transition = []
    for line in reader:
        probabilities = [float(item) for item in line.split()[1:]]
        transition.append(probabilities)

    hmm = HMM(alphabet=None, states=states, transition=Transition(states, transition), emission=None)
    return hidden_path, hmm


def calc_probability_of_hidden_path(hidden_path, hmm):
    transition_matrix = hmm.transition
    initial_transition_probability = 1.0 / hmm.states.nr_states
    log_p = math.log(initial_transition_probability)
    for s1, s2 in zip(hidden_path, hidden_path[1:]):
        log_p += math.log(transition_matrix[s1, s2])
    return math.exp(log_p)


def main():
    reader = sys.stdin
    path, hmm = read_data(reader)
    result = calc_probability_of_hidden_path(path, hmm)
    print(result)


if __name__ == '__main__':
    main()
