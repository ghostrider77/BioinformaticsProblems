# Implement TreeColoring
import sys

from enum import Enum


class NodeColor(Enum):
    BLUE = 'blue'
    RED = 'red'
    PURPLE = 'purple'


def read_edges(reader):
    adjacency_list = {}
    while (line := next(reader).rstrip()) != '-':
        node, neighbours = line.split(' -> ')
        if neighbours != '{}':
            adjacency_list[int(node)] = tuple(map(int, neighbours.split(',')))
    return adjacency_list


def read_leaf_colors(reader):
    node_colors = {}
    for line in reader:
        node, color = line.split(': ')
        node_colors[int(node)] = NodeColor(color.rstrip())
    return node_colors


def collect_ripe_nodes(adjacency_list, node_colors):
    ripe_nodes = []
    for node, children in adjacency_list.items():
        if node not in node_colors and all(child in node_colors for child in children):
            ripe_nodes.append(node)
    return ripe_nodes


def assign_color_to_node(node, adjacency_list, node_colors):
    children = adjacency_list[node]
    child_colors = {node_colors[child] for child in children}
    if len(child_colors) == 1:
        return next(iter(child_colors))

    return NodeColor.PURPLE


def perform_node_coloring(adjacency_list, node_colors):
    if not adjacency_list:
        return node_colors

    while ripe_nodes := collect_ripe_nodes(adjacency_list, node_colors):
        for node in ripe_nodes:
            node_colors[node] = assign_color_to_node(node, adjacency_list, node_colors)
    return node_colors


def main():
    reader = sys.stdin
    adjacency_list = read_edges(reader)
    node_colors = read_leaf_colors(reader)
    result = perform_node_coloring(adjacency_list, node_colors)
    for node, color in result.items():
        print(f'{node}: {color.value}')


if __name__ == '__main__':
    main()
