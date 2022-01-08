# 2-Satisfiability
import itertools as it
import sys

from collections import defaultdict, namedtuple

DFSResult = namedtuple('DFSResult', ['components', 'topological_ordering', 'previsit_numbers', 'postvisit_numbers'])


class Node:
    def __init__(self, original_id, nr_nodes):
        self._nr_nodes = nr_nodes
        self._node_id = self._encode_id(original_id)
        self._original_id = original_id

    def __repr__(self):
        return str(self._original_id)

    def __eq__(self, that):
        return self.original_id == that.original_id

    def __lt__(self, that):
        return abs(self.original_id) < abs(that.original_id)

    def __hash__(self):
        return hash(self._original_id)

    @property
    def node_id(self):
        return self._node_id

    @property
    def original_id(self):
        return self._original_id

    def __neg__(self):
        return Node(-self._original_id, self._nr_nodes)

    def _encode_id(self, original_id):
        if original_id < 0:
            return -original_id + self._nr_nodes
        return original_id


class DirectedGraph:
    def __init__(self, nr_nodes, edge_list, ordered_nodes=None):
        self._nr_nodes = nr_nodes
        self._edge_list = edge_list
        self._adjacency_list = self._build_adjacency_list(edge_list)
        if ordered_nodes is None:
            ordered_nodes = (Node(self._original_id(node_id), self.nr_nodes // 2) for node_id in range(1, nr_nodes+1))

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
    def edge_list(self):
        return self._edge_list

    @property
    def ordered_nodes(self):
        return self._ordered_nodes

    def _original_id(self, node_id):
        n = self.nr_nodes // 2
        if node_id <= n:
            return node_id

        return -(node_id - n)

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
            if previsit_numbers[neighbour.node_id-1] == 0:
                return neighbour
        return None

    def explore(starting_node):
        previsit_numbers[starting_node.node_id-1] = next(previsit_id)
        previsit_stack = [starting_node]
        current_component = {starting_node}
        while previsit_stack:
            last_node = previsit_stack.pop(-1)
            unvisited_neighbour = find_unvisited_neighbour_of_a_node(last_node)
            if unvisited_neighbour is None:
                postvisit_numbers[last_node.node_id-1] = next(postvisit_id)
                topological_ordering.append(last_node)
            else:
                previsit_numbers[unvisited_neighbour.node_id-1] = next(previsit_id)
                previsit_stack.append(last_node)
                previsit_stack.append(unvisited_neighbour)
                current_component.add(unvisited_neighbour)
        return current_component

    components = []
    for starting_node in graph.ordered_nodes:
        if previsit_numbers[starting_node.node_id-1] == 0:
            current_component = explore(starting_node)
            components.append(frozenset(current_component))
    return DFSResult(components, topological_ordering[::-1], previsit_numbers, postvisit_numbers)


def convert_to_intlist(line):
    return tuple(int(item) for item in line.split())


def read_edges(reader, nr_nodes, nr_edges):
    edges = []
    for _ in range(nr_edges):
        a, b = convert_to_intlist(next(reader))
        node1 = Node(a, nr_nodes)
        node2 = Node(b, nr_nodes)
        edges.append((-node1, node2))
        edges.append((-node2, node1))
    return edges


def read_graphs(reader, n):
    graphs = []
    for _ in range(n):
        _ = next(reader)
        nr_nodes, nr_edges = convert_to_intlist(next(reader))
        edge_list = read_edges(reader, nr_nodes, nr_edges)
        graph = DirectedGraph(2*nr_nodes, edge_list)
        graphs.append(graph)
    return graphs


def create_graph_with_edges_reversed(graph, postvisit_numbers):
    reversed_edges = [(b, a) for a, b in graph.edge_list]
    node_order, _ = zip(*sorted(zip(graph.ordered_nodes, postvisit_numbers), key=lambda x: x[1], reverse=True))
    return DirectedGraph(graph.nr_nodes, reversed_edges, node_order)


def calc_strongly_connected_components(graph):
    dfs_result = depth_first_search(graph)
    reversed_graph = create_graph_with_edges_reversed(graph, dfs_result.postvisit_numbers)
    reversed_dfs = depth_first_search(reversed_graph)
    return reversed_dfs.components


def calc_component_assignment(component, literal_assignment):
    component_assignment = set()
    for node in component:
        if (-node) in component_assignment:
            return None
        if node not in literal_assignment and (-node) not in literal_assignment:
            component_assignment.add(node)
    return component_assignment


def solve_2sat_problem(graph):
    components = calc_strongly_connected_components(graph)
    literal_assignment = set()
    for component in reversed(components):
        if (component_assignment := calc_component_assignment(component, literal_assignment)) is None:
            return None
        literal_assignment.update(component_assignment)
    return literal_assignment


def main():
    reader = sys.stdin
    nr_examples = int(next(reader))
    graphs = read_graphs(reader, nr_examples)
    result = map(solve_2sat_problem, graphs)
    for line in result:
        if line is None:
            print('0')
        else:
            sorted_result = ' '.join(map(str, sorted(line)))
            print(f'1 {sorted_result}')


if __name__ == '__main__':
    main()
