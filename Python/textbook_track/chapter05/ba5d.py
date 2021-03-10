# Find the Longest Path in a DAG
import functools as ft
import sys

from collections import defaultdict
from itertools import count
from math import inf


class DirectedWeightedGraph:
    def __init__(self, adjacency_list):
        self._adjacency_list = adjacency_list
        self._nodes = self._collect_graph_nodes()
        self._incoming_edges = self._calc_incoming_edges()

        self._previsit_numbers = {}
        self._previsit_id = count(start=1)

    @property
    def incoming_edges(self):
        return self._incoming_edges

    @ft.cached_property
    def topological_ordering(self):
        return self._run_depth_first_search()

    def _collect_graph_nodes(self):
        nodes = set()
        for node, neighbours in self._adjacency_list.items():
            nodes.add(node)
            for neighbour, _ in neighbours:
                nodes.add(neighbour)
        return frozenset(nodes)

    def _calc_incoming_edges(self):
        ingraph = defaultdict(list)
        for node, neighbours in self._adjacency_list.items():
            for neighbour, weight in neighbours:
                ingraph[neighbour].append((node, weight))
        return dict(ingraph)

    def _run_depth_first_search(self):
        topological_sorting = []
        for node in self._nodes:
            if not self._is_visited_in_dfs(node):
                self._explore(node, topological_sorting)
        return topological_sorting[::-1]

    def _is_visited_in_dfs(self, node):
        return self._previsit_numbers.get(node, 0) > 0

    def _find_unvisited_neighbour_of_a_node(self, node):
        neighbours = self._adjacency_list.get(node, [])
        for neighbour, _ in neighbours:
            if not self._is_visited_in_dfs(neighbour):
                return neighbour
        return None

    def _explore(self, starting_node, topological_sorting):
        self._previsit_numbers[starting_node] = next(self._previsit_id)
        previsit_stack = [starting_node]
        while previsit_stack:
            last_node = previsit_stack.pop(-1)
            unvisited_neighbour = self._find_unvisited_neighbour_of_a_node(last_node)
            if unvisited_neighbour is None:
                topological_sorting.append(last_node)
            else:
                self._previsit_numbers[unvisited_neighbour] = next(self._previsit_id)
                previsit_stack.append(last_node)
                previsit_stack.append(unvisited_neighbour)


def read_weighted_edges(lines):
    adjacency_list = defaultdict(list)
    for line in lines:
        node_from, node_to, weight = map(int, line.replace('->', ':').split(':'))
        adjacency_list[node_from].append((node_to, weight))
    return dict(adjacency_list)


def collect_longest_path(backtrack, source, sink):
    path = [sink]
    node = sink
    while node != source:
        node = backtrack[node]
        path.append(node)
    return path[::-1]


def find_longest_path(graph, source, sink):
    longest_path = {}
    backtrack = {}
    for node in graph.topological_ordering:
        predecessors = graph.incoming_edges.get(node, [])
        if node == source:
            longest_path[node] = 0
        elif predecessors:
            paths_from_predecessors = ((u, longest_path.get(u, -inf) + w) for u, w in predecessors)
            v, s = max(paths_from_predecessors, key=lambda x: x[1])
            longest_path[node] = s
            backtrack[node] = v

    return longest_path[sink], collect_longest_path(backtrack, source, sink)


def main():
    data = sys.stdin.read().splitlines()
    source = int(data[0])
    sink = int(data[1])
    adjacency_list = read_weighted_edges(data[2:])
    graph = DirectedWeightedGraph(adjacency_list)
    length, longest_path = find_longest_path(graph, source, sink)
    print(length)
    print('->'.join(map(str, longest_path)))


if __name__ == '__main__':
    main()
