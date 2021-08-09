# Construct a Suffix Tree from a Suffix Array
import itertools as it
import sys

from collections import defaultdict, namedtuple


Node = namedtuple('Node', ['node_id', 'parent_id', 'depth'])
Edge = namedtuple('Edge', ['node_to', 'substring_start', 'substring_length'])


class SuffixTree:
    def __init__(self, text, suffix_array, lcp_array):
        self._text = text
        self._size = len(text)
        self._node_id_generator = it.count()
        self._adjacency_list = self._build_suffix_tree(suffix_array, lcp_array)

    def edges(self):
        for edge_labels in self._adjacency_list.values():
            for _, start_ix, length in edge_labels:
                yield self._text[start_ix:start_ix+length]

    def _build_suffix_tree(self, suffix_array, lcp_array):
        adjacency_list = defaultdict(list)
        current_node_id = next(self._node_id_generator)
        current_node = Node(node_id=current_node_id, parent_id=None, depth=0)
        nodes = {current_node_id: current_node}

        for suffix_start, lcp in zip(suffix_array, lcp_array):
            current_node = find_starting_node_for_next_edge(nodes, current_node, lcp)
            substring_start = suffix_start + current_node.depth
            if current_node.depth == lcp:
                current_node = self._add_new_leaf(adjacency_list, current_node, nodes, substring_start)
            else:
                edge = self._identify_edge(adjacency_list, current_node, substring_start)
                offset = lcp - current_node.depth
                middle_node = self._split_edge(current_node, edge, offset, nodes, adjacency_list)
                current_node = self._add_new_leaf(adjacency_list, middle_node, nodes, substring_start+offset)

        return dict(adjacency_list)

    def _split_edge(self, node, edge, offset, nodes, adjacency_list):
        end_node = nodes[edge.node_to]
        middle_node_id = next(self._node_id_generator)
        middle_node = Node(node_id=middle_node_id, parent_id=node.node_id, depth=node.depth+offset)
        nodes[middle_node_id] = middle_node
        nodes[end_node.node_id] = Node(node_id=end_node.node_id, parent_id=middle_node_id, depth=end_node.depth)

        first_edge = Edge(middle_node_id, edge.substring_start, offset)
        second_edge = Edge(edge.node_to, edge.substring_start+offset, edge.substring_length-offset)
        adjacency_list[middle_node_id].append(second_edge)
        adjacency_list[node.node_id].append(first_edge)
        adjacency_list[node.node_id].remove(edge)
        return middle_node

    def _add_new_leaf(self, adjacency_list, node, nodes, substring_start):
        leaf_id = next(self._node_id_generator)
        label_length = self._size - substring_start
        leaf = Node(node_id=leaf_id, parent_id=node.node_id, depth=node.depth+label_length)
        nodes[leaf_id] = leaf
        adjacency_list[node.node_id].append(Edge(leaf_id, substring_start, label_length))
        return leaf

    def _identify_edge(self, adjacency_list, current_node, substring_start):
        char = self._text[substring_start]
        for edge in adjacency_list[current_node.node_id]:
            if self._text[edge.substring_start] == char:
                return edge
        return None


def find_starting_node_for_next_edge(nodes, node, lcp):
    while node.depth > lcp:
        node = nodes[node.parent_id]
    return node


def convert_to_intlist(line):
    return [int(elem) for elem in line.split(', ')]


def main():
    reader = sys.stdin
    text = next(reader).rstrip()
    suffix_array = convert_to_intlist(next(reader))
    lcp_array = convert_to_intlist(next(reader))
    suffix_tree = SuffixTree(text, suffix_array, lcp_array)
    for edge_label in suffix_tree.edges():
        print(edge_label)


if __name__ == '__main__':
    main()
