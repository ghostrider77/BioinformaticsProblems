# Construct a Trie from a Collection of Patterns
import itertools as it
import sys

from collections import defaultdict, namedtuple

Edge = namedtuple('Edge', ['node_to', 'substring_start', 'substring_length'])


class SuffixTree:
    def __init__(self, text):
        self._text = text
        self._size = len(text)
        self._node_id_generator = it.count()
        self._root = next(self._node_id_generator)
        self._adjacency_list = self._build_suffix_tree()

    def edges(self):
        for edge_labels in self._adjacency_list.values():
            for _, start_ix, length in edge_labels:
                yield self._text[start_ix:start_ix+length]

    def _build_suffix_tree(self):
        adjacency_list = defaultdict(list)
        for ix in range(self._size):
            self._add_suffix_to_tree(adjacency_list, ix)
        return dict(adjacency_list)

    def _add_suffix_to_tree(self, adjacency_list, suffix_start):
        node, edge, edge_start_ix, non_matching_ix = self._find_last_matching_node(adjacency_list, suffix_start)
        new_leaf = next(self._node_id_generator)
        if edge is None:
            adjacency_list[node].append(Edge(new_leaf, edge_start_ix, self._size - edge_start_ix))
        else:
            new_node = next(self._node_id_generator)
            split_length_1 = non_matching_ix - edge_start_ix
            split_length_2 = edge.substring_length - split_length_1
            adjacency_list[node].remove(edge)
            adjacency_list[node].append(Edge(new_node, edge.substring_start, split_length_1))
            adjacency_list[new_node].append(Edge(edge.node_to, edge.substring_start+split_length_1, split_length_2))
            new_edge = Edge(new_leaf, non_matching_ix, self._size - non_matching_ix)
            adjacency_list[new_node].append(new_edge)

    def _find_last_matching_node(self, adjacency_list, suffix_start):
        ix = suffix_start
        current_node = self._root
        while (edge := self._find_next_edge_in_path(adjacency_list, current_node, self._text[ix])) is not None:
            non_matching_ix = self._find_first_non_matching_index(ix, edge)
            if non_matching_ix is not None:
                return current_node, edge, ix, non_matching_ix

            current_node = edge.node_to
            ix += edge.substring_length

        return current_node, None, ix, ix

    def _find_next_edge_in_path(self, adjacency_list, node, letter):
        edges = adjacency_list.get(node, [])
        for edge in edges:
            if self._text[edge.substring_start] == letter:
                return edge
        return None

    def _find_first_non_matching_index(self, ix, edge):
        substring = self._text[edge.substring_start:edge.substring_start+edge.substring_length]
        for jy, (a, b) in enumerate(zip(self._text[ix:], substring), start=ix):
            if a != b:
                return jy
        return None


def main():
    reader = sys.stdin
    text = next(reader).rstrip()
    tree = SuffixTree(text)
    for edge_label in tree.edges():
        print(edge_label)


if __name__ == '__main__':
    main()
