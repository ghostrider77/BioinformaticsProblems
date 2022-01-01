# Hamiltonian Path in DAG
import functools as ft
import itertools as it
import sys

from collections import defaultdict


class DirectedGraph:
    def __init__(self, nr_nodes, edge_list):
        self._nr_nodes = nr_nodes
        self._adjacency_list = self._build_adjacency_list(edge_list)
        self._topological_ordering = None

        self._previsit_numbers = [0] * nr_nodes
        self._postvisit_numbers = [0] * nr_nodes
        self._previsit_id = it.count(1)
        self._postvisit_id = it.count(1)

    @staticmethod
    def _build_adjacency_list(edge_list):
        adjacency_list = defaultdict(list)
        for a, b in edge_list:
            adjacency_list[a].append(b)
        return dict(adjacency_list)

    @ft.cached_property
    def topological_sorting(self):
        _ = self._depth_first_search()
        return tuple(self._topological_ordering[::-1])

    def has_edge(self, node1, node2):
        return node2 in self._adjacency_list.get(node1, [])

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
        for neighbour in neighbours:
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
        a, b = convert_to_intlist(next(reader))
        edges.append((a, b))
    return edges


def read_graphs(reader, n):
    graphs = []
    for _ in range(n):
        nr_nodes, nr_edges = convert_to_intlist(next(reader))
        edge_list = read_edges(reader, nr_edges)
        graph = DirectedGraph(nr_nodes, edge_list)
        graphs.append(graph)
    return graphs


def find_Hamiltonian_path(graph):
    ordered_nodes = graph.topological_sorting
    if all(graph.has_edge(node1, node2) for node1, node2 in zip(ordered_nodes, ordered_nodes[1:])):
        return ordered_nodes
    return None


def main():
    reader = sys.stdin
    nr_examples = int(next(reader))
    graphs = read_graphs(reader, nr_examples)
    results = map(find_Hamiltonian_path, graphs)
    for result in results:
        if result is None:
            print(-1)
        else:
            print(f"1 {' '.join(map(str, result))}")


if __name__ == '__main__':
    main()
