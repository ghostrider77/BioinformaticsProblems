# Dijkstra's Algorithm
import sys

from collections import defaultdict, namedtuple
from math import inf, isinf

Edge = namedtuple('Edge', ['node_from', 'node_to', 'weight'])


class DirectedGraph:
    def __init__(self, nr_nodes, edge_list):
        self._nr_nodes = nr_nodes
        self._adjacency_list = self._build_adjacency_list(edge_list)

    @staticmethod
    def _build_adjacency_list(edge_list):
        adjacency_list = defaultdict(list)
        for edge in edge_list:
            adjacency_list[edge.node_from].append(edge)
        return dict(adjacency_list)

    @property
    def nr_nodes(self):
        return self._nr_nodes

    def neighbours(self, node):
        return self._adjacency_list.get(node, [])


def convert_to_intlist(line):
    return tuple(int(item) for item in line.split())


def read_edges(reader, nr_edges):
    edges = []
    for _ in range(nr_edges):
        a, b, w = convert_to_intlist(next(reader))
        edges.append(Edge(node_from=a, node_to=b, weight=w))
    return edges


def update_distances(neighbours, dist_u, distances, nodes):
    for _, v, weight in neighbours:
        distance_through_u = dist_u + weight
        if distances[v-1] > distance_through_u:
            distances[v-1] = distance_through_u
            nodes[v] = distance_through_u


def calc_shortest_distances(graph, source_node):
    distances = [inf] * graph.nr_nodes
    distances[source_node-1] = 0
    nodes = {node: distances[node-1] for node in range(1, graph.nr_nodes+1)}
    while nodes:
         u = min(nodes, key=nodes.get)
         dist_u = nodes.pop(u)
         neighbours = graph.neighbours(u)
         update_distances(neighbours, dist_u, distances, nodes)

    return tuple(-1 if isinf(d) else d for d in distances)


def main():
    reader = sys.stdin
    nr_nodes, nr_edges = convert_to_intlist(next(reader))
    edge_list = read_edges(reader, nr_edges)
    graph = DirectedGraph(nr_nodes, edge_list)
    result = calc_shortest_distances(graph, source_node=1)
    print(' '.join(map(str, result)))


if __name__ == '__main__':
    main()
