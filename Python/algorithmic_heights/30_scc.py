# Strongly Connected Components
import itertools as it
import sys

from collections import defaultdict, namedtuple

DFSResult = namedtuple('DFSResult', ['components', 'topological_ordering', 'previsit_numbers', 'postvisit_numbers'])


class DirectedGraph:
    def __init__(self, nr_nodes, edge_list, ordered_nodes=None):
        self._nr_nodes = nr_nodes
        self._adjacency_list = self._build_adjacency_list(edge_list)
        if ordered_nodes is None:
            ordered_nodes = range(1, nr_nodes+1)

        self._ordered_nodes = tuple(ordered_nodes)

    @staticmethod
    def _build_adjacency_list(edge_list):
        adjacency_list = defaultdict(list)
        for a, b in edge_list:
            adjacency_list[a].append(b)
        return dict(adjacency_list)

    @property
    def nr_nodes(self):
        return self._nr_nodes

    @property
    def ordered_nodes(self):
        return self._ordered_nodes

    def neighbours(self, node):
        return self._adjacency_list.get(node, [])


def depth_first_search(graph):
    previsit_numbers = [0] * graph.nr_nodes
    postvisit_numbers = [0] * graph.nr_nodes
    previsit_id = it.count(1)
    postvisit_id = it.count(1)
    topological_ordering = []

    def find_unvisited_neighbour_of_a_node(node):
        for neighbour in graph.neighbours(node):
            if previsit_numbers[neighbour-1] == 0:
                return neighbour
        return None

    def explore(starting_node):
        previsit_numbers[starting_node-1] = next(previsit_id)
        previsit_stack = [starting_node]
        current_component = {starting_node}
        while previsit_stack:
            last_node = previsit_stack.pop(-1)
            unvisited_neighbour = find_unvisited_neighbour_of_a_node(last_node)
            if unvisited_neighbour is None:
                postvisit_numbers[last_node-1] = next(postvisit_id)
                topological_ordering.append(last_node)
            else:
                previsit_numbers[unvisited_neighbour-1] = next(previsit_id)
                previsit_stack.append(last_node)
                previsit_stack.append(unvisited_neighbour)
                current_component.add(unvisited_neighbour)
        return current_component

    components = []
    for starting_node in graph.ordered_nodes:
        if previsit_numbers[starting_node-1] == 0:
            current_component = explore(starting_node)
            components.append(frozenset(current_component))
    return DFSResult(components, topological_ordering[::-1], previsit_numbers, postvisit_numbers)


def convert_to_intlist(line):
    return tuple(int(item) for item in line.split())


def read_edges(reader, nr_edges):
    edges = []
    for _ in range(nr_edges):
        a, b = convert_to_intlist(next(reader))
        edges.append((a, b))
    return edges


def create_graph_with_edges_reversed(nr_nodes, edge_list, postvisit_numbers):
    reversed_edges = ([(b, a) for a, b in edge_list])
    node_order, _ = zip(*sorted(zip(range(1, nr_nodes+1), postvisit_numbers), key=lambda x: x[1], reverse=True))
    return DirectedGraph(nr_nodes, reversed_edges, node_order)


def calc_strongly_connected_components(nr_nodes, edge_list):
    graph = DirectedGraph(nr_nodes, edge_list)
    dfs_result = depth_first_search(graph)
    reversed_graph = create_graph_with_edges_reversed(nr_nodes, edge_list, dfs_result.postvisit_numbers)
    reversed_dfs = depth_first_search(reversed_graph)
    return reversed_dfs.components


def main():
    reader = sys.stdin
    nr_nodes, nr_edges = convert_to_intlist(next(reader))
    edge_list = read_edges(reader, nr_edges)
    components = calc_strongly_connected_components(nr_nodes, edge_list)
    print(len(components))


if __name__ == '__main__':
    main()
