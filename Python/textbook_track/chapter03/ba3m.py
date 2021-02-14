# Generate All Maximal Non-Branching Paths in a Graph
import itertools as it
import functools as ft
import sys

from collections import defaultdict


class Graph:
    def __init__(self, adjacency_list):
        self._adjacency_list = adjacency_list
        self._in_degrees, self._out_degrees = self._calc_node_degrees()

    @ft.cached_property
    def nodes(self):
        nodes = set()
        for node, neighbours in self._adjacency_list.items():
            nodes.add(node)
            for neighbour in neighbours:
                nodes.add(neighbour)
        return frozenset(nodes)

    def neighbours(self, node):
        return self._adjacency_list.get(node, [])

    def in_degree(self, node):
        return self._in_degrees.get(node, 0)

    def out_degree(self, node):
        return self._out_degrees.get(node, 0)

    def is_node_one_in_one_out(self, node):
        return self.in_degree(node) == 1 and self.out_degree(node) == 1

    def _calc_node_degrees(self):
        in_degrees = defaultdict(int)
        out_degrees = defaultdict(int)
        for node, neighbours in self._adjacency_list.items():
            for neighbour in neighbours:
                out_degrees[node] += 1
                in_degrees[neighbour] += 1
        return in_degrees, out_degrees


def convert_to_intlist(line, sep=None):
    return [int(elem) for elem in line.split(sep)]


def read_lines(lines):
    d = {}
    for line in lines:
        node, neighbours = line.split(' -> ')
        d[int(node)] = convert_to_intlist(neighbours, sep=',')
    return d


def build_nonbranching_paths(graph, node):
    paths = []
    for neighbour in graph.neighbours(node):
        w = neighbour
        path = [node, w]
        while graph.is_node_one_in_one_out(w):
            w = graph.neighbours(w)[0]
            path.append(w)
        paths.append(tuple(path))
    return paths


def find_nodes_with_outgoing_edge(graph):
    return {node for node in graph.nodes if graph.out_degree(node) > 0}


def build_cycle(graph, start_node):
    cycle = [start_node]
    node = start_node
    while (node := graph.neighbours(node)[0]) != start_node:
        cycle.append(node)
    cycle.append(start_node)
    return cycle


def find_isolated_cycles_in_graph(graph, found_paths):
    used_nodes = set(it.chain.from_iterable(found_paths))
    missing_nodes = set.difference(find_nodes_with_outgoing_edge(graph), used_nodes)
    isolated_cycles = []
    while missing_nodes:
        node = missing_nodes.pop()
        cycle = build_cycle(graph, node)
        isolated_cycles.append(tuple(cycle))
        missing_nodes.difference_update(cycle)
    return isolated_cycles


def find_maximal_non_branching_paths(graph):
    paths = []
    for node in graph.nodes:
        if (not graph.is_node_one_in_one_out(node)) and graph.out_degree(node) > 0:
            paths_from_node = build_nonbranching_paths(graph, node)
            paths.extend(paths_from_node)
    cycles = find_isolated_cycles_in_graph(graph, paths)
    paths.extend(cycles)
    return paths


def main():
    data = sys.stdin.read().splitlines()
    adjacency_list = read_lines(data)
    graph = Graph(adjacency_list)
    result = find_maximal_non_branching_paths(graph)
    for path in result:
        print(' -> '.join(map(str, path)))


if __name__ == '__main__':
    main()
