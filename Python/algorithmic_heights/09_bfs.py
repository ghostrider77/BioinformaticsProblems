# Double-Degree Array
import sys

from collections import defaultdict, deque


class DirectedGraph:
    def __init__(self, nr_nodes, edge_list):
        self._nr_nodes = nr_nodes
        self._adjacency_list = self._build_adjacency_list(edge_list)

    @staticmethod
    def _build_adjacency_list(edge_list):
        adjacency_list = defaultdict(list)
        for a, b in edge_list:
            adjacency_list[a].append(b)
        return dict(adjacency_list)

    def breadth_first_search(self, start_node):
        distances = [-1] * self._nr_nodes
        distances[start_node-1] = 0
        queue = deque([start_node])
        while queue:
            node = queue.popleft()
            neighbours = self._adjacency_list.get(node, [])
            for neighbour in neighbours:
                if distances[neighbour-1] == -1:
                    queue.append(neighbour)
                    distances[neighbour-1] = distances[node-1] + 1
        return distances


def convert_to_intlist(line):
    return tuple(int(item) for item in line.split())


def read_edges(reader, nr_edges):
    edges = []
    for _ in range(nr_edges):
        a, b = convert_to_intlist(next(reader))
        edges.append((a, b))
    return edges


def main():
    reader = sys.stdin
    nr_nodes, nr_edges = convert_to_intlist(next(reader))
    edge_list = read_edges(reader, nr_edges)
    graph = DirectedGraph(nr_nodes, edge_list)
    result = graph.breadth_first_search(start_node=1)
    print(' '.join(map(str, result)))


if __name__ == '__main__':
    main()
