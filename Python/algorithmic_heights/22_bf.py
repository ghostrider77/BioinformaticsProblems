# Bellman-Ford Algorithm
import sys

from collections import defaultdict, namedtuple
from math import inf, isinf, isfinite

Edge = namedtuple('Edge', ['node_from', 'node_to', 'weight'])


class DirectedGraph:
    def __init__(self, nr_nodes, edge_list):
        self._nr_nodes = nr_nodes
        self._adjacency_list = self._build_adjacency_list(edge_list)

    def __iter__(self):
        yield from self._adjacency_list.items()

    @staticmethod
    def _build_adjacency_list(edge_list):
        adjacency_list = defaultdict(list)
        for edge in edge_list:
            adjacency_list[edge.node_from].append(edge)
        return dict(adjacency_list)

    @property
    def nr_nodes(self):
        return self._nr_nodes


def convert_to_intlist(line):
    return tuple(int(item) for item in line.split())


def read_edges(reader, nr_edges):
    edges = []
    for _ in range(nr_edges):
        a, b, w = convert_to_intlist(next(reader))
        edges.append(Edge(node_from=a, node_to=b, weight=w))
    return edges


def update_distances(graph, distances):
    for node, neighbours in graph:
        dist_node = distances[node-1]
        if isfinite(dist_node):
            for _, neighbour, weight in neighbours:
                distance_through_node = dist_node + weight
                if distances[neighbour-1] > distance_through_node:
                    distances[neighbour-1] = distance_through_node


def bellman_ford_algorithm(graph, node):
    n = graph.nr_nodes
    distances = [inf] * n
    distances[node-1] = 0
    for _ in range(n):
        update_distances(graph, distances)
    return distances


def main():
    reader = sys.stdin
    nr_nodes, nr_edges = convert_to_intlist(next(reader))
    edge_list = read_edges(reader, nr_edges)
    graph = DirectedGraph(nr_nodes, edge_list)
    result = bellman_ford_algorithm(graph, node=1)
    print(' '.join(map(lambda x: 'x' if isinf(x) else str(x), result)))


if __name__ == '__main__':
    main()
