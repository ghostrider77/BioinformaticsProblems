# Find a k-Universal Circular String
import itertools as it
import sys

from collections import defaultdict


def create_adjacency_list_from_binary_strings(k):
    d = defaultdict(list)
    for k_mer in map(''.join, it.product('01', repeat=k)):
        prefix = k_mer[:-1]
        suffix = k_mer[1:]
        d[prefix].append(suffix)
    return dict(d)


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


def find_eulerian_cycle(adjacency_list):
    start_node = min(adjacency_list)
    cycle = extend_cycle(adjacency_list, start_node)
    while adjacency_list:
        ix, new_start_node = find_node_with_unused_edges_on_cycle(adjacency_list, cycle)
        shifted_cycle = shift_cycle(cycle, ix)
        cycle = extend_cycle(adjacency_list, new_start_node, shifted_cycle)
    return cycle


def calc_string_spelled_by_a_genome_path(k_mers):
    text = list(k_mers[0])
    for k_mer in k_mers[1:]:
        text.append(k_mer[-1])
    return ''.join(text)


def find_universal_circular_binary_string(k):
    if k <= 0:
        return ''

    if k == 1:
        return '01'

    adjacency_list = create_adjacency_list_from_binary_strings(k)
    cycle = find_eulerian_cycle(adjacency_list)
    universal_string = calc_string_spelled_by_a_genome_path(cycle)
    return universal_string[:-(k-1)]


def main():
    reader = sys.stdin
    k = int(next(reader))
    result = find_universal_circular_binary_string(k)
    print(result)


if __name__ == '__main__':
    main()
