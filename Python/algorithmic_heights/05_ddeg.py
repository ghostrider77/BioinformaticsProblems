# Double-Degree Array
import sys

from collections import defaultdict


class Graph:
    def __init__(self, nr_nodes, edge_list):
        self._nr_nodes = nr_nodes
        self._adjacency_list = self._build_adjacency_list(edge_list)
        self._degrees = self._calc_node_degrees()

    @staticmethod
    def _build_adjacency_list(edge_list):
        adjacency_list = defaultdict(list)
        for a, b in edge_list:
            adjacency_list[a].append(b)
            adjacency_list[b].append(a)
        return dict(adjacency_list)

    def _calc_node_degrees(self):
        return {node: len(neighbours) for node, neighbours in self._adjacency_list.items()}

    def neighbours(self, node):
        return self._adjacency_list.get(node, [])

    def degree(self, node):
        return self._degrees.get(node, 0)


def convert_to_intlist(line):
    return tuple(int(item) for item in line.split())


def read_edges(reader, nr_edges):
    edges = []
    for _ in range(nr_edges):
        a, b = convert_to_intlist(next(reader))
        edges.append((a, b))
    return edges


def calc_double_degree_array(nr_nodes, edge_list):
    graph = Graph(nr_nodes, edge_list)
    double_degrees = []
    for node_id in range(1, nr_nodes+1):
        degree_sum = sum(graph.degree(neighbour) for neighbour in graph.neighbours(node_id))
        double_degrees.append(degree_sum)
    return double_degrees


def main():
    reader = sys.stdin
    nr_nodes, nr_edges = convert_to_intlist(next(reader))
    edge_list = read_edges(reader, nr_edges)
    result = calc_double_degree_array(nr_nodes, edge_list)
    print(' '.join(map(str, result)))


if __name__ == '__main__':
    main()
