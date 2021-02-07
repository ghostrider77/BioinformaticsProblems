# Reconstruct a String from its k-mer Composition
import sys

from collections import defaultdict


def build_de_bruijn_graph(k_mers):
    d = defaultdict(list)
    for k_mer in k_mers:
        prefix = k_mer[:-1]
        suffix = k_mer[1:]
        d[prefix].append(suffix)
    return dict(d)


def calc_node_degrees(adjacency_list):
    degrees = defaultdict(int)
    for node, neighbours in adjacency_list.items():
        for neighbour in neighbours:
            degrees[node] += 1
            degrees[neighbour] -= 1
    return dict(degrees)


def find_eulerian_path_endpoints(adjacency_list):
    degrees = calc_node_degrees(adjacency_list)

    def find_node(value):
        for node, degree in degrees.items():
            if degree == value:
                return node

    path_start = find_node(value=1)
    path_end = find_node(value=-1)
    return path_start, path_end


def add_extra_edge(adjacency_list, path_start, path_end):
    neighbours = adjacency_list.setdefault(path_end, [])
    neighbours.append(path_start)


def find_node_with_unused_edges_on_cycle(adjacency_list, cycle):
    for ix, node in enumerate(cycle):
        if node in adjacency_list:
            return ix, node


def shift_cycle(cycle, ix):
    next_start_node = cycle[ix]
    _ = cycle.pop(-1)
    new_cycle = cycle[ix:] + cycle[:ix]
    new_cycle.append(next_start_node)
    return new_cycle


def extend_cycle(adjacency_list, start_node, cycle=None):
    if cycle is None:
        cycle = [start_node]

    current_node = start_node
    while current_node in adjacency_list:
        neighbours = adjacency_list[current_node]
        next_node = neighbours.pop(-1)
        if not neighbours:
            _ = adjacency_list.pop(current_node)

        cycle.append(next_node)
        current_node = next_node
    return cycle


def remove_extra_edge_from_cycle(cycle, path_start, path_end):
    for ix, (a, b) in enumerate(zip(cycle, cycle[1:])):
        if a == path_end and b == path_start:
            return cycle[ix+1:] + cycle[1:ix+1]
    return cycle


def find_eulerian_path(adjacency_list):
    path_start, path_end = find_eulerian_path_endpoints(adjacency_list)
    add_extra_edge(adjacency_list, path_start, path_end)

    start_node = min(adjacency_list)
    cycle = extend_cycle(adjacency_list, start_node)
    while adjacency_list:
        ix, new_start_node = find_node_with_unused_edges_on_cycle(adjacency_list, cycle)
        shifted_cycle = shift_cycle(cycle, ix)
        cycle = extend_cycle(adjacency_list, new_start_node, shifted_cycle)
    return remove_extra_edge_from_cycle(cycle, path_start, path_end)


def calc_string_spelled_by_a_genome_path(k_mers):
    text = list(k_mers[0])
    for k_mer in k_mers[1:]:
        text.append(k_mer[-1])
    return ''.join(text)


def main():
    data = sys.stdin.read().splitlines()
    k_mers = data[1:]
    adjacency_list = build_de_bruijn_graph(k_mers)
    path = find_eulerian_path(adjacency_list)
    result = calc_string_spelled_by_a_genome_path(path)
    print(result)


if __name__ == '__main__':
    main()
