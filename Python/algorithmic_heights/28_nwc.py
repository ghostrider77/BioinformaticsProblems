# Negative Weight Cycle
import sys

from collections import namedtuple

Edge = namedtuple('Edge', ['node_from', 'node_to', 'weight'])


class DirectedGraph:
    def __init__(self, nr_nodes, edge_list):
        self._nr_nodes = nr_nodes
        self._edge_list = edge_list

    def __iter__(self):
        yield from self._edge_list

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


def read_graphs(reader, n):
    graphs = []
    for _ in range(n):
        nr_nodes, nr_edges = convert_to_intlist(next(reader))
        edge_list = read_edges(reader, nr_edges)
        graph = DirectedGraph(nr_nodes, edge_list)
        graphs.append(graph)
    return graphs


def update_distances(graph, distances):
    update_flag = False
    for node, neighbour, weight in graph:
        distance_through_node = distances[node-1] + weight
        if distances[neighbour-1] > distance_through_node:
            distances[neighbour-1] = distance_through_node
            update_flag = True
    return update_flag


def has_negative_cycle(graph):
    n = graph.nr_nodes
    distances = [0] * n
    for k in range(n):
         is_updated = update_distances(graph, distances)
         if is_updated and k == n - 1:
             return True
    return False


def main():
    reader = sys.stdin
    nr_examples = int(next(reader))
    graphs = read_graphs(reader, nr_examples)
    result = map(has_negative_cycle, graphs)
    print(' '.join(map(lambda x: '1' if x else '-1', result)))


if __name__ == '__main__':
    main()
