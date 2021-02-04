# Construct the Overlap Graph of a Collection of k-mers
import sys

from collections import defaultdict


class OverlapGraph:
    def __init__(self, k_mers):
        self._adjacency_list = build_overlap_graph(k_mers)

    def edges(self):
        for k_mer, neighbours in self._adjacency_list.items():
            for neighbour in neighbours:
                yield f'{k_mer} -> {neighbour}'


def build_overlap_graph(k_mers):
    def create_prefix_to_pattern_dict():
        d = defaultdict(list)
        for k_mer in k_mers:
            prefix = k_mer[:-1]
            d[prefix].append(k_mer)
        return dict(d)

    prefix_to_pattern = create_prefix_to_pattern_dict()
    graph = defaultdict(list)
    for k_mer in k_mers:
        suffix = k_mer[1:]
        neighbours = prefix_to_pattern.get(suffix, [])
        graph[k_mer].extend(neighbours)
    return dict(graph)


def main():
    data = sys.stdin.read().splitlines()
    graph = OverlapGraph(data)
    for edge in graph.edges():
        print(edge)


if __name__ == '__main__':
    main()
