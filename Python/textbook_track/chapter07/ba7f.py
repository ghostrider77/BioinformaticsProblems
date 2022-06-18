# Implement SmallParsimony
import itertools as it
import math
import sys

from collections import defaultdict

NUCLEOTIDES = ('A', 'C', 'G', 'T')


class Tree:
    def __init__(self, adjacency_list, nr_leaves):
        self._adjacency_list = adjacency_list
        self._nr_leaves = nr_leaves

        nr_nodes = max(self._adjacency_list.keys()) + 1
        self._nodes = tuple(range(nr_nodes))
        self._parents = self._calc_parents(nr_nodes)
        self._root = self._get_root_node_id()

    def _calc_parents(self, nr_nodes):
        parents = [-1] * nr_nodes
        for parent, children in self._adjacency_list.items():
            for child in children:
                parents[child] = parent
        return tuple(parents)

    def _get_root_node_id(self):
        for node, parent in enumerate(self._parents):
            if parent == -1:
                return node

    @property
    def nr_nodes(self):
        return len(self._nodes)

    @property
    def nodes(self):
        return self._nodes

    @property
    def root(self):
        return self._root

    def get_children(self, node):
        return self._adjacency_list.get(node, [])

    def get_parent(self, node):
        return self._parents[node]


def read_input_data(lines):
    leaf_id_generator = it.count(0)
    adjacency_list = defaultdict(list)
    characters = []
    for node_id, neighbour in map(lambda x: x.split("->"), lines):
        if neighbour.isnumeric():
            adjacency_list[int(node_id)].append(int(neighbour))
        else:
            adjacency_list[int(node_id)].append(next(leaf_id_generator))
            characters.append(neighbour)
    return dict(adjacency_list), characters


def calc_hamming_distance(s1, s2):
    distance = 0
    for c1, c2 in zip(s1, s2):
        if c1 != c2:
            distance += 1
    return distance


def initialize_minimum_parsimony_score_for_leaves(leaf_labels):
    minimum_parsimony_score = {}
    for leaf_id, label in enumerate(leaf_labels):
        minimum_parsimony_score[leaf_id] = tuple(0 if label == nucleotide else math.inf for nucleotide in NUCLEOTIDES)
    return minimum_parsimony_score


def get_ripe_nodes(tree, minimum_parsimony_scores):
    ripe_nodes = set()
    for node in tree.nodes:
        if children := tree.get_children(node):
            if all(child in minimum_parsimony_scores for child in children):
                ripe_nodes.add(node)
    return ripe_nodes


def calc_parsimony_scores_for_node(tree, minimum_parsimony_scores, node):
    (left_child, right_child) = tree.get_children(node)
    left_child_scores = minimum_parsimony_scores[left_child]
    right_child_scores = minimum_parsimony_scores[right_child]

    def calc_child_tree_score(node_label, child_scores):
        return min(score if char == node_label else score + 1 for char, score in zip(NUCLEOTIDES, child_scores))

    scores = []
    for nucleotide in NUCLEOTIDES:
        left = calc_child_tree_score(nucleotide, left_child_scores)
        right = calc_child_tree_score(nucleotide, right_child_scores)
        scores.append(left + right)

    return tuple(scores)


def get_most_parsimonious_labeling(tree, minimum_parsimony_scores):
    def get_label(scores):
        _, label = min(zip(scores, NUCLEOTIDES), key=lambda x: x[0])
        return label

    node_labels = [""] * tree.nr_nodes
    root_node = tree.root
    node_labels[root_node] = get_label(minimum_parsimony_scores[root_node])
    stack = list(tree.get_children(root_node))
    while stack:
        node_id = stack.pop(-1)
        parent_label = node_labels[tree.get_parent(node_id)]
        node_scores = minimum_parsimony_scores[node_id]
        modified_scores = [score if char == parent_label else score + 1
                           for char, score in zip(NUCLEOTIDES, node_scores)]
        node_labels[node_id] = get_label(modified_scores)
        stack.extend(tree.get_children(node_id))

    return node_labels


def solve_small_parsimony_for_single_label(tree, leaf_labels):
    minimum_parsimony_scores = initialize_minimum_parsimony_score_for_leaves(leaf_labels)
    ripe_nodes = get_ripe_nodes(tree, minimum_parsimony_scores)
    while ripe_nodes:
        node = ripe_nodes.pop()
        minimum_parsimony_scores[node] = calc_parsimony_scores_for_node(tree, minimum_parsimony_scores, node)
        if (parent_node := tree.get_parent(node)) != -1:
            children = tree.get_children(parent_node)
            if all(child in minimum_parsimony_scores for child in children):
                ripe_nodes.add(parent_node)

    return minimum_parsimony_scores


def solve_small_parsimony_problem(tree, characters):
    parsimony_score = 0
    node_labels = []
    for leaf_labels in zip(*characters):
        minimum_parsimony_scores = solve_small_parsimony_for_single_label(tree, leaf_labels)
        parsimony_score += min(minimum_parsimony_scores[tree.root])
        labels = get_most_parsimonious_labeling(tree, minimum_parsimony_scores)
        node_labels.append(labels)
    node_labels = tuple(map("".join, zip(*node_labels)))
    return parsimony_score, node_labels


def create_edge_output(tree, node_labels):
    for node_id in tree.nodes:
        node_label = node_labels[node_id]
        for child_id in tree.get_children(node_id):
            child_label = node_labels[child_id]
            distance = calc_hamming_distance(node_label, child_label)
            yield f"{node_label}->{child_label}:{distance}"
            yield f"{child_label}->{node_label}:{distance}"


def main():
    data = sys.stdin.read().splitlines()
    nr_leaves = int(data[0])
    adjacency_list, characters = read_input_data(data[1:])
    tree = Tree(adjacency_list, nr_leaves)
    parsimony_score, node_labels = solve_small_parsimony_problem(tree, characters)
    print(parsimony_score)
    edges = create_edge_output(tree, node_labels)
    for edge in edges:
        print(edge)


if __name__ == '__main__':
    main()
