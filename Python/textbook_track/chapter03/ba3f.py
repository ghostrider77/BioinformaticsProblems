# Find an Eulerian Cycle in a Graph
import sys


def convert_to_intlist(line, sep=None):
    return [int(elem) for elem in line.split(sep)]


def read_lines(lines):
    d = {}
    for line in lines:
        node, neighbours = line.split(' -> ')
        d[int(node)] = convert_to_intlist(neighbours, sep=',')
    return d


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


def main():
    data = sys.stdin.read().splitlines()
    adjacency_list = read_lines(data)
    result = find_eulerian_cycle(adjacency_list)
    print(len(result))
    print('->'.join(map(str, result)))


if __name__ == '__main__':
    main()
