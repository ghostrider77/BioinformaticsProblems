# Testing Bipartiteness
import sys

from collections import defaultdict, deque
from enum import Enum, auto


class NodeColor(Enum):
    RED = auto()
    BLUE = auto()


class Graph:
    def __init__(self, nr_nodes, edge_list):
        self._nr_nodes = nr_nodes
        self._adjacency_list = self._build_adjacency_list(edge_list)

    @staticmethod
    def _build_adjacency_list(edge_list):
        adjacency_list = defaultdict(list)
        for a, b in edge_list:
            adjacency_list[a].append(b)
            adjacency_list[b].append(a)
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


def get_opposite_color(color):
    if color == NodeColor.RED:
        return NodeColor.BLUE

    return NodeColor.RED


def find_consistent_component_coloring(graph, starting_node):
    node_colors_in_component = {starting_node: NodeColor.RED}
    queue = deque([starting_node])
    while queue:
        node = queue.popleft()
        node_color = node_colors_in_component[node]
        color = get_opposite_color(node_color)
        neighbours = graph.neighbours(node)
        for neighbour in neighbours:
            if neighbour in node_colors_in_component:
                if node_colors_in_component[neighbour] == node_color:
                    return None
            else:
                node_colors_in_component[neighbour] = color
                queue.append(neighbour)
    return node_colors_in_component


def is_bipartite(graph):
    node_colors = {}
    for starting_node in range(1, graph.nr_nodes+1):
        if starting_node not in node_colors:
            component_coloring = find_consistent_component_coloring(graph, starting_node)
            if component_coloring is None:
                return False

            node_colors.update(component_coloring)
    return True


def test_bipartiteness(graphs):
    return [is_bipartite(graph) for graph in graphs]


def main():
    reader = sys.stdin
    n = int(next(reader))
    graphs = read_graphs(reader, n)
    result = test_bipartiteness(graphs)
    print(' '.join(map(lambda x: '1' if x else '-1', result)))


if __name__ == '__main__':
    main()
