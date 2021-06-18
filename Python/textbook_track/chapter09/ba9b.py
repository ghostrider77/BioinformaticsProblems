# Implement TrieMatching
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

    def is_matching(self, text):
        current_node = self._root
        for letter in text:
            next_node = _get_neighbour_with_given_label(self._adjacency_list, current_node, letter)
            if next_node is None:
                return False

            if self._is_pattern_end(next_node):
                return True

            current_node = next_node

        return False

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

    def _is_pattern_end(self, node):
        neighbours = self._adjacency_list.get(node, [])
        return any(label == '$' for _, label in neighbours)


def _get_neighbour_with_given_label(adjacency_list, current_node, letter):
    neighbours = adjacency_list.get(current_node, [])
    for node, label in neighbours:
        if label == letter:
            return node
    return None


def run_trie_matching(text, patterns):
    trie = Trie(patterns)
    matching_indices = []
    for ix in range(len(text)):
        if trie.is_matching(text[ix:]):
            matching_indices.append(ix)
    return matching_indices


def main():
    data = sys.stdin.read().splitlines()
    text = data[0]
    patterns = map(lambda x: x + '$', data[1:])
    result = run_trie_matching(text, patterns)
    print(' '.join(map(str, result)))


if __name__ == '__main__':
    main()
