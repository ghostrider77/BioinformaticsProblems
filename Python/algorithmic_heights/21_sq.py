# Square in a Graph
import sys

from collections import defaultdict


class Graph:
    def __init__(self, nr_nodes, edge_list):
        self._nr_nodes = nr_nodes
        self._adjacency_list = self._build_adjacency_list(edge_list)

    @staticmethod
    def _build_adjacency_list(edge_list):
        adjacency_list = defaultdict(set)
        for a, b in edge_list:
            adjacency_list[a].add(b)
            adjacency_list[b].add(a)
        return dict(adjacency_list)

    @property
    def nr_nodes(self):
        return self._nr_nodes

    def neighbours(self, node):
        return self._adjacency_list.get(node, set())


def convert_to_intlist(line):
    return tuple(int(item) for item in line.split())


def read_edges(reader, nr_edges):
    edges = []
    for _ in range(nr_edges):
        a, b = convert_to_intlist(next(reader))
        edges.append((a, b))
    return edges


def read_graphs(reader, n):
    graphs = []
    for _ in range(n):
        _ = next(reader)
        nr_nodes, nr_edges = convert_to_intlist(next(reader))
        edge_list = read_edges(reader, nr_edges)
        graph = Graph(nr_nodes, edge_list)
        graphs.append(graph)
    return graphs


def has_cycle_of_length_four(graph):
    for node1 in range(1, graph.nr_nodes+1):
        if (neighbours1 := graph.neighbours(node1)):
            for node2 in range(node1+1, graph.nr_nodes+1):
                neighbours2 = graph.neighbours(node2)
                if len(neighbours1 & neighbours2) >= 2:
                    return True
    return False


def have_graphs_squares(graphs):
    return [has_cycle_of_length_four(graph) for graph in graphs]


def main():
    reader = sys.stdin
    nr_examples = int(next(reader))
    graphs = read_graphs(reader, nr_examples)
    result = have_graphs_squares(graphs)
    print(' '.join(map(lambda x: '1' if x else '-1', result)))


if __name__ == '__main__':
    main()
