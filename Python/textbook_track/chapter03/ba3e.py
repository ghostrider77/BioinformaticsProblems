# Construct the De Bruijn Graph of a Collection of k-mers
import sys

from collections import defaultdict


class DeBruijnGraph:
    def __init__(self, k_mers):
        self._adjacency_list = build_de_bruijn_graph(k_mers)

    def edges(self):
        for prefix, neighbours in self._adjacency_list.items():
            neighbour_string = ','.join(neighbours)
            yield f'{prefix} -> {neighbour_string}'


def build_de_bruijn_graph(k_mers):
    d = defaultdict(list)
    for k_mer in k_mers:
        prefix = k_mer[:-1]
        suffix = k_mer[1:]
        d[prefix].append(suffix)
    return dict(d)


def main():
    data = sys.stdin.read().splitlines()
    graph = DeBruijnGraph(data)
    for edge in graph.edges():
        print(edge)


if __name__ == '__main__':
    main()
