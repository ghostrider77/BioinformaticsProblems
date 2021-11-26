# Shortest Cycle Through a Given Edge
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


def read_graphs(reader, n):
    graphs = []
    given_edges = []
    for _ in range(n):
        _ = next(reader)
        nr_nodes, nr_edges = convert_to_intlist(next(reader))
        edge_list = read_edges(reader, nr_edges)
        graph = DirectedGraph(nr_nodes, edge_list)
        graphs.append(graph)
        given_edges.append(edge_list[0])
    return graphs, given_edges


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
    return distances


def calc_shortest_cycle_through_given_edge(graphs, given_edges):
    shortest_cycle_lengths = []
    for graph, edge in zip(graphs, given_edges):
        distances = calc_shortest_distances(graph, edge.node_to)
        dist = distances[edge.node_from-1]
        if isinf(dist):
            shortest_cycle_lengths.append(-1)
        else:
            shortest_cycle_lengths.append(dist + edge.weight)
    return shortest_cycle_lengths


def main():
    reader = sys.stdin
    nr_examples = int(next(reader))
    graphs, given_edges = read_graphs(reader, nr_examples)
    result = calc_shortest_cycle_through_given_edge(graphs, given_edges)
    print(' '.join(map(str, result)))


if __name__ == '__main__':
    main()
