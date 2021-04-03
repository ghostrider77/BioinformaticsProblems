# Implement AdditivePhylogeny
import sys

from collections import deque
from itertools import count
from math import inf


class Tree:
    def __init__(self, adjacency_list, nr_leaves, nr_initial_leaves=2):
        self._adjacency_list = adjacency_list
        self._inner_node_generator = count(nr_leaves)
        self._leaf_generator = count(nr_initial_leaves)

    def edges(self):
        ordered_nodes = sorted(self._adjacency_list.keys())
        for node in ordered_nodes:
            neighbours = self._adjacency_list[node]
            for neighbour, weight in neighbours:
                yield f'{node}->{neighbour}:{weight}'

    def insert_leaf_into_tree(self, limblength, leaf_start, leaf_end, insertion_distance):
        path = self._breadth_first_search(leaf_start, leaf_end)
        current_node = leaf_start
        path_length = 0
        for next_node, weight in path:
            path_length += weight
            if insertion_distance < path_length:
                distance_from_current_node = weight - (path_length - insertion_distance)
                return self._insert_leaf(current_node, next_node, weight, distance_from_current_node, limblength)
            current_node = next_node

    def _breadth_first_search(self, start_leaf, end_leaf):
        distances = {start_leaf: 0}
        backtrack = {}
        queue = deque([start_leaf])
        while queue:
            node = queue.popleft()
            neighbours = self._adjacency_list.get(node, [])
            for neighbour, weight in neighbours:
                if neighbour not in distances:
                    queue.append(neighbour)
                    distances[neighbour] = distances[node] + weight
                    backtrack[neighbour] = (node, weight)
        return self._resolve_path_between_leaves(backtrack, start_leaf, end_leaf)

    @staticmethod
    def _resolve_path_between_leaves(backtrack, start_leaf, end_leaf):
        path = []
        node = end_leaf
        while node != start_leaf:
            previous_node, weight = backtrack[node]
            path.append((node, weight))
            node = previous_node
        return path[::-1]

    def _insert_leaf(self, current_node, next_node, weight, distance_from_current_node, limblength):
        next_leaf = next(self._leaf_generator)
        if distance_from_current_node == 0:
            self._adjacency_list[next_leaf] = [(current_node, limblength)]
            self._adjacency_list[current_node].append((next_leaf, limblength))
        else:
            middle_node = next(self._inner_node_generator)
            self._add_middle_node(current_node, next_node, middle_node, distance_from_current_node)
            self._add_middle_node(next_node, current_node, middle_node, weight - distance_from_current_node)
            self._adjacency_list[middle_node] = [(current_node, distance_from_current_node),
                                                 (next_node, weight - distance_from_current_node),
                                                 (next_leaf, limblength)]
            self._adjacency_list[next_leaf] = [(middle_node, limblength)]

    def _add_middle_node(self, current_node, neighbour, middle_node, distance):
        all_neighbours = self._adjacency_list[current_node]
        new_neighbours = [(node, weight) for node, weight in all_neighbours if node != neighbour]
        new_neighbours.append((middle_node, distance))
        self._adjacency_list[current_node] = new_neighbours


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def read_distance_matrix(reader, nr_leaves):
    matrix = []
    for _ in range(nr_leaves):
        row = convert_to_intlist(next(reader))
        matrix.append(row)
    return matrix


def calc_limb_length(distances, leaf_j, nr_leaves):
    limb_length = inf
    other_leaves = [leaf for leaf in range(nr_leaves) if leaf != leaf_j]
    for leaf_i in other_leaves:
        for leaf_k in other_leaves:
            candidate = (distances[leaf_i][leaf_j] + distances[leaf_j][leaf_k] - distances[leaf_i][leaf_k]) // 2
            if candidate < limb_length:
                limb_length = candidate
    return limb_length


def subtract_limb_length(distances, leaf, limblength):
    bald_distances = []
    for ix, row in enumerate(distances):
        updated_row = []
        for jy, elem in enumerate(row):
            item = elem
            if ix == leaf or jy == leaf:
                item -= limblength
            updated_row.append(item)
        bald_distances.append(updated_row)
    bald_distances[leaf][leaf] = 0
    return bald_distances


def find_path_where_leaf_should_be_attached(distances, leaf):
    size = len(distances)
    for leaf_i in range(size):
        for leaf_k in range(size):
            if distances[leaf_i][leaf_k] == distances[leaf_i][leaf] + distances[leaf][leaf_k]:
                return leaf_i, leaf_k


def remove_last_row_and_column(distances):
    _ = distances.pop(-1)
    for row in distances:
        _ = row.pop(-1)


def create_initial_tree(distance, nr_leaves):
    adjacency_list = {0: [(1, distance)], 1: [(0, distance)]}
    return Tree(adjacency_list, nr_leaves)


def iteratively_reduce_distance_matrix(distances, nr_leaves):
    limblengths = [None] * nr_leaves
    attachment_points = [None] * nr_leaves
    for last_leaf in range(nr_leaves-1, 1, -1):
        limblength = calc_limb_length(distances, last_leaf, last_leaf+1)
        limblengths[last_leaf] = limblength
        bald_distances = subtract_limb_length(distances, last_leaf, limblength)
        leaf_i, leaf_k = find_path_where_leaf_should_be_attached(bald_distances, last_leaf)
        dist = bald_distances[leaf_i][last_leaf]
        attachment_points[last_leaf] = (leaf_i, leaf_k, dist)
        remove_last_row_and_column(bald_distances)
        distances = bald_distances
    initial_tree = create_initial_tree(distances[0][1], nr_leaves)
    return initial_tree, limblengths, attachment_points


def additive_phylogeny(distances, nr_leaves):
    tree, limblengths, attachment_points = iteratively_reduce_distance_matrix(distances, nr_leaves)
    for limblength, (leaf_i, leaf_k, distance) in zip(limblengths[2:], attachment_points[2:]):
        tree.insert_leaf_into_tree(limblength, leaf_i, leaf_k, distance)
    return tree


def main():
    reader = sys.stdin
    nr_leaves = int(next(reader))
    distances = read_distance_matrix(reader, nr_leaves)
    result = additive_phylogeny(distances, nr_leaves)
    for edge in result.edges():
        print(edge)


if __name__ == '__main__':
    main()
