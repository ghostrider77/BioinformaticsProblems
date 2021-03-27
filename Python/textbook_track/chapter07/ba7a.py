# Compute Distances Between Leaves
import sys

from collections import defaultdict
from math import inf


def read_weighted_edges(lines):
    adjacency_list = defaultdict(list)
    for line in lines:
        node_from, node_to, weight = map(int, line.replace('->', ':').split(':'))
        adjacency_list[node_from].append((node_to, weight))
    return dict(adjacency_list)


def generate_nodes(adjacency_list):
    for node, neighbours in adjacency_list.items():
        for neighbour, _ in neighbours:
            yield neighbour


def initialize_distance_matrix(adjacency_list, nr_nodes):
    distances = [[inf] * nr_nodes for _ in range(nr_nodes)]
    for node, neighbours in adjacency_list.items():
        for neighbour, weight in neighbours:
            distances[node][neighbour] = weight

    for node in range(nr_nodes):
        distances[node][node] = 0

    return distances


def calc_pairwise_distances(adjacency_list):
    nodes = generate_nodes(adjacency_list)
    nr_nodes = max(nodes) + 1
    distances = initialize_distance_matrix(adjacency_list, nr_nodes)
    for k in range(nr_nodes):
        for ix in range(nr_nodes):
            for jy in range(nr_nodes):
                distances[ix][jy] = min(distances[ix][jy], distances[ix][k] + distances[k][jy])
    return distances


def compute_distances_between_leaves(adjacency_list, nr_leaves):
    distances = calc_pairwise_distances(adjacency_list)
    return [row[:nr_leaves] for row in distances[:nr_leaves]]


def main():
    data = sys.stdin.read().splitlines()
    nr_leaves = int(data[0])
    adjacency_list = read_weighted_edges(data[1:])
    result = compute_distances_between_leaves(adjacency_list, nr_leaves)
    for row in result:
        print(' '.join(map(str, row)))


if __name__ == '__main__':
    main()
