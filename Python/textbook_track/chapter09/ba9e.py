# Find the Longest Substring Shared by Two Strings
import functools as ft
import itertools as it
import sys

from collections import defaultdict, namedtuple
from enum import Enum, auto

Edge = namedtuple('Edge', ['node_to', 'substring_start', 'substring_length'])


class NodeColor(Enum):
    BLUE = auto()
    RED = auto()
    PURPLE = auto()
    GRAY = auto()


class SuffixTree:
    def __init__(self, text1, text2):
        self._text = text1 + text2
        self._text1_size = len(text1)
        self._size = len(self._text)
        self._node_id_generator = it.count()
        self._root = next(self._node_id_generator)
        self._adjacency_list = self._build_suffix_tree()

    @property
    def text(self):
        return self._text

    @property
    def root(self):
        return self._root

    @property
    def adjacency_list(self):
        return self._adjacency_list

    @ft.cached_property
    def nr_nodes(self):
        n = 0
        for edges in self._adjacency_list.values():
            for edge in edges:
                if edge.node_to > n:
                    n = edge.node_to
        return n + 1

    def does_edge_belong_to_first_text(self, edge):
        return edge.substring_start < self._text1_size

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


def perform_node_coloring(suffix_tree):
    node_colors = initialize_node_colors(suffix_tree)
    while (ripe_nodes := find_ripe_nodes(suffix_tree, node_colors)):
        for node in ripe_nodes:
            children = suffix_tree.adjacency_list.get(node, [])
            colors = {node_colors[edge.node_to] for edge in children}
            if len(colors) == 1:
                node_colors[node] = next(iter(colors))
            else:
                node_colors[node] = NodeColor.PURPLE
    return tuple(node_colors)


def initialize_node_colors(suffix_tree):
    node_colors = [NodeColor.GRAY] * suffix_tree.nr_nodes
    current_nodes = [(suffix_tree.root, NodeColor.GRAY)]
    while current_nodes:
        next_nodes = []
        for node, node_color in current_nodes:
            edges = suffix_tree.adjacency_list.get(node, [])
            if edges:
                neighbours = collect_edge_endpoints(suffix_tree, edges)
                next_nodes.extend(neighbours)
            else:
                node_colors[node] = node_color
        current_nodes = next_nodes
    return node_colors


def collect_edge_endpoints(suffix_tree, edges):
    neighbours = []
    for edge in edges:
        if suffix_tree.does_edge_belong_to_first_text(edge):
            node_color = NodeColor.BLUE
        else:
            node_color = NodeColor.RED
        neighbours.append((edge.node_to, node_color))
    return neighbours


def find_ripe_nodes(suffix_tree, node_colors):
    ripe_nodes = []
    for node, color in enumerate(node_colors):
        if color == NodeColor.GRAY:
            children = suffix_tree.adjacency_list.get(node, [])
            if all(node_colors[edge.node_to] != NodeColor.GRAY for edge in children):
                ripe_nodes.append(node)
    return ripe_nodes


def find_longest_substring_spelled_by_purple_nodes(suffix_tree, node_colors):
    longest_shared_substring = ''
    current_nodes = [(suffix_tree.root, longest_shared_substring)]
    while current_nodes:
        next_nodes = []
        for node, spelled_string in current_nodes:
            next_nodes.extend(get_purple_neighbour_nodes(suffix_tree, node_colors, node, spelled_string))

        longest_shared_substring = update_longest_shared_substring(longest_shared_substring, next_nodes)
        current_nodes = next_nodes

    return longest_shared_substring


def get_purple_neighbour_nodes(suffix_tree, node_colors, node, spelled_string):
    purple_children = []
    edges = suffix_tree.adjacency_list.get(node, [])
    for neighbour, substring_start, substring_length in edges:
        if node_colors[neighbour] == NodeColor.PURPLE:
            edge_label = suffix_tree.text[substring_start:substring_start+substring_length]
            purple_children.append((neighbour, spelled_string + edge_label))
    return purple_children


def update_longest_shared_substring(longest_shared_substring, nodes):
    length = len(longest_shared_substring)
    for _, string in nodes:
        if len(string) > length:
            longest_shared_substring = string
            length = len(longest_shared_substring)

    return longest_shared_substring


def find_longest_shared_substring(text1, text2):
    suffix_tree = SuffixTree(text1 + '#', text2 + '$')
    node_colors = perform_node_coloring(suffix_tree)
    return find_longest_substring_spelled_by_purple_nodes(suffix_tree, node_colors)


def main():
    reader = sys.stdin
    text1 = next(reader).rstrip()
    text2 = next(reader).rstrip()
    result = find_longest_shared_substring(text1, text2)
    print(result)


if __name__ == '__main__':
    main()
