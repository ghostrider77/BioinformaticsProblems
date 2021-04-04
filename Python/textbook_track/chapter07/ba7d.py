# Implement UPGMA
import sys

from collections import namedtuple
from itertools import count

Node = namedtuple('Node', ['id', 'age', 'size'])


class DistanceMatrix:
    def __init__(self, distances):
        self._distances = distances

    def __getitem__(self, key):
        i, j = key
        if i < j:
            return self._distances[(i, j)]
        if i == j:
            return 0
        return self._distances[(j, i)]

    def __setitem__(self, key, value):
        i, j = key
        if i < j:
            self._distances[(i, j)] = value
        elif i > j:
            self._distances[(j, i)] = value

    def _current_cluster_ids(self):
        ids = set()
        for key in self._distances.keys():
            ids.update(key)
        return frozenset(ids)

    def closest_clusters(self):
        return min(self._distances, key=self._distances.get)

    def update(self, node_1, node_2, next_id):
        cluster_ids = self._current_cluster_ids()
        for cluster_id in cluster_ids:
            num = self[(cluster_id, node_1.id)] * node_1.size + self[(cluster_id, node_2.id)] * node_2.size
            self[(cluster_id, next_id)] = num / (node_1.size + node_2.size)

        self._distances = {key: dist for key, dist in self._distances.items()
                           if node_1.id not in key and node_2.id not in key}


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def read_distance_matrix(reader, nr_leaves):
    distances = {}
    for ix in range(nr_leaves):
        row = convert_to_intlist(next(reader))
        for jy in range(ix+1, nr_leaves):
            distances[(ix, jy)] = row[jy]
    return DistanceMatrix(distances)


def get_tree_edges(adjacency_list):
    edges = []
    for node, neighbours in adjacency_list.items():
        for neighbour in neighbours:
            weight = round(node.age - neighbour.age, 3)
            edges.append((node.id, neighbour.id, weight))
            edges.append((neighbour.id, node.id, weight))
    return sorted(edges, key=lambda x: x[0])


def perform_upgma_clustering(distances, nr_leaves):
    id_generator = count(nr_leaves)
    clusters = {k: Node(id=k, age=0, size=1) for k in range(nr_leaves)}
    adjacency_list = {}
    while len(clusters) > 1:
        id_1, id_2 = distances.closest_clusters()
        dist = distances[(id_1, id_2)]
        node_1 = clusters.pop(id_1)
        node_2 = clusters.pop(id_2)
        next_id = next(id_generator)
        next_node = Node(id=next_id, age=dist/2, size=node_1.size+node_2.size)
        clusters[next_id] = next_node
        adjacency_list[next_node] = (node_1, node_2)
        distances.update(node_1, node_2, next_id)
    return get_tree_edges(adjacency_list)


def main():
    reader = sys.stdin
    nr_leaves = int(next(reader))
    distances = read_distance_matrix(reader, nr_leaves)
    result = perform_upgma_clustering(distances, nr_leaves)
    for node1, node2, weight in result:
        print(f'{node1}->{node2}:{weight}')


if __name__ == '__main__':
    main()
