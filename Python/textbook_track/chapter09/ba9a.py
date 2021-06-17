# Construct a Trie from a Collection of Patterns
import itertools as it
import sys

from collections import defaultdict


class Trie:
    def __init__(self, patterns):
        self._node_id_generator = it.count()
        self._root = next(self._node_id_generator)
        self._adjacency_list = self._build_trie_from_patterns(patterns)

    def edges(self):
        for node, neighbours in self._adjacency_list.items():
            for neighbour, label in neighbours:
                yield f'{node}->{neighbour}:{label}'

    def _build_trie_from_patterns(self, patterns):
        adjacency_list = defaultdict(list)
        for pattern in patterns:
            self._add_pattern_to_trie(adjacency_list, pattern)
        return dict(adjacency_list)

    def _add_pattern_to_trie(self, adjacency_list, pattern):
        current_node = self._root
        for letter in pattern:
            node = _get_neighbour_with_given_label(adjacency_list, current_node, letter)
            if node is None:
                next_node = next(self._node_id_generator)
                adjacency_list[current_node].append((next_node, letter))
                current_node = next_node
            else:
                current_node = node


def _get_neighbour_with_given_label(adjacency_list, current_node, letter):
    neighbours = adjacency_list.get(current_node, [])
    for node, label in neighbours:
        if label == letter:
            return node
    return None


def main():
    patterns = sys.stdin.read().splitlines()
    trie = Trie(patterns)
    for edge in trie.edges():
        print(edge)


if __name__ == '__main__':
    main()
