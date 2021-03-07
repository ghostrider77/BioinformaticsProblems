# Find a topological ordering of a directed acyclic graph.
import functools as ft
import sys

from itertools import count


class DirectedGraph:
    def __init__(self, adjacency_list):
        self._adjacency_list = adjacency_list
        self._nodes = self._collect_graph_nodes()

        self._previsit_numbers = {}
        self._previsit_id = count(start=1)

    @ft.cached_property
    def topological_ordering(self):
        return self._run_depth_first_search()

    def _collect_graph_nodes(self):
        nodes = set()
        for node, neighbours in self._adjacency_list.items():
            nodes.add(node)
            nodes.update(neighbours)
        return frozenset(nodes)

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
        for neighbour in neighbours:
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


def create_adjacency_list(lines):
    adjacency_list = {}
    for line in lines:
        node, neighbours = line.split('->')
        adjacency_list[int(node)] = list(map(int, neighbours.split(',')))
    return adjacency_list


def main():
    data = sys.stdin.read().splitlines()
    adjacency_list = create_adjacency_list(data)
    graph = DirectedGraph(adjacency_list)
    result = graph.topological_ordering
    print(', '.join(map(str, result)))


if __name__ == '__main__':
    main()
