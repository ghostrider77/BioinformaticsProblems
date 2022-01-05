# Shortest Paths in DAG
import functools as ft
import itertools as it
import math
import sys

from collections import defaultdict, namedtuple

Edge = namedtuple('Edge', ['node_from', 'node_to', 'weight'])


class DirectedGraph:
    def __init__(self, nr_nodes, edge_list):
        self._nr_nodes = nr_nodes
        self._adjacency_list = self._build_adjacency_list(edge_list)
        self._topological_ordering = None

        self._previsit_numbers = [0] * nr_nodes
        self._postvisit_numbers = [0] * nr_nodes
        self._previsit_id = it.count(1)
        self._postvisit_id = it.count(1)

    @property
    def nr_nodes(self):
        return self._nr_nodes

    @staticmethod
    def _build_adjacency_list(edge_list):
        adjacency_list = defaultdict(list)
        for edge in edge_list:
            adjacency_list[edge.node_from].append(edge)
        return dict(adjacency_list)

    @ft.cached_property
    def topological_sorting(self):
        _ = self._depth_first_search()
        return tuple(self._topological_ordering[::-1])

    def neighbours(self, node):
        return self._adjacency_list.get(node, [])

    def _depth_first_search(self):
        self._topological_ordering = []
        components = []
        for starting_node in range(1, self._nr_nodes+1):
            if not self._is_visited_in_dfs(starting_node):
                current_component = self._explore(starting_node)
                components.append(frozenset(current_component))
        return tuple(components)

    def _is_visited_in_dfs(self, node):
        return self._previsit_numbers[node-1] > 0

    def _find_unvisited_neighbour_of_a_node(self, node):
        neighbours = self._adjacency_list.get(node, [])
        for _, neighbour, _ in neighbours:
            if not self._is_visited_in_dfs(neighbour):
                return neighbour
        return None

    def _explore(self, starting_node):
        self._previsit_numbers[starting_node-1] = next(self._previsit_id)
        previsit_stack = [starting_node]
        current_component = {starting_node}
        while previsit_stack:
            last_node = previsit_stack.pop(-1)
            unvisited_neighbour = self._find_unvisited_neighbour_of_a_node(last_node)
            if unvisited_neighbour is None:
                self._postvisit_numbers[last_node-1] = next(self._postvisit_id)
                self._topological_ordering.append(last_node)
            else:
                self._previsit_numbers[unvisited_neighbour-1] = next(self._previsit_id)
                previsit_stack.append(last_node)
                previsit_stack.append(unvisited_neighbour)
                current_component.add(unvisited_neighbour)
        return current_component


def convert_to_intlist(line):
    return tuple(int(item) for item in line.split())


def read_edges(reader, nr_edges):
    edges = []
    for _ in range(nr_edges):
        a, b, w = convert_to_intlist(next(reader))
        edges.append(Edge(node_from=a, node_to=b, weight=w))
    return edges


def find_shortest_path(graph, start_node):
    distances = [math.inf] * graph.nr_nodes
    distances[start_node-1] = 0
    node_order = graph.topological_sorting
    for node in it.dropwhile(lambda x: x != start_node, node_order):
        shortest_path_to_node = distances[node-1]
        for _, neighbour, weight in graph.neighbours(node):
            if distances[neighbour-1] > shortest_path_to_node + weight:
                distances[neighbour-1] = shortest_path_to_node + weight
    return distances


def main():
    reader = sys.stdin
    nr_nodes, nr_edges = convert_to_intlist(next(reader))
    edge_list = read_edges(reader, nr_edges)
    graph = DirectedGraph(nr_nodes, edge_list)
    result = find_shortest_path(graph, start_node=1)
    print(' '.join(['x' if math.isinf(item) else str(item) for item in result]))


if __name__ == '__main__':
    main()
