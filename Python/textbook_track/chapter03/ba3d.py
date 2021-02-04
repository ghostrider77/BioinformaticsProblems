# Construct the De Bruijn Graph of a String
import sys

from collections import defaultdict


class DeBruijnGraph:
    def __init__(self, text, k):
        self._adjacency_list = build_de_bruijn_graph(text, k)

    def edges(self):
        for prefix, neighbours in self._adjacency_list.items():
            neighbour_string = ','.join(neighbours)
            yield f'{prefix} -> {neighbour_string}'


def build_de_bruijn_graph(text, k):
    d = defaultdict(list)
    for ix in range(len(text)-k+1):
        k_mer = text[ix:ix+k]
        prefix = k_mer[:-1]
        suffix = k_mer[1:]
        d[prefix].append(suffix)
    return dict(d)


def main():
    data = sys.stdin.read().splitlines()
    k = int(data[0])
    text = data[1]
    graph = DeBruijnGraph(text, k)
    for edge in graph.edges():
        print(edge)


if __name__ == '__main__':
    main()
