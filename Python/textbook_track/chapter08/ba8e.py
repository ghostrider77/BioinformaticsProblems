import sys

from collections import namedtuple
from itertools import count

Cluster = namedtuple('Cluster', ['id', 'content'])


class DistanceMatrix:
    def __init__(self, distances):
        self._distances = distances

    def __getitem__(self, key):
        i, j = key
        if i < j:
            return self._distances[(i, j)]
        if i == j:
            return 0
        return self._distances[(j, i)]

    def __setitem__(self, key, value):
        i, j = key
        if i < j:
            self._distances[(i, j)] = value
        elif i > j:
            self._distances[(j, i)] = value

    def _current_cluster_ids(self):
        ids = set()
        for key in self._distances.keys():
            ids.update(key)
        return frozenset(ids)

    def closest_clusters(self):
        return min(self._distances, key=self._distances.get)

    def update(self, cluster_1, cluster_2, next_id):
        cluster_ids = self._current_cluster_ids()
        for cluster_id in cluster_ids:
            size_1 = len(cluster_1.content)
            size_2 = len(cluster_2.content)
            num = self[(cluster_id, cluster_1.id)] * size_1 + self[(cluster_id, cluster_2.id)] * size_2
            self[(cluster_id, next_id)] = num / (size_1 + size_2)

        self._distances = {key: dist for key, dist in self._distances.items()
                           if cluster_1.id not in key and cluster_2.id not in key}


def convert_to_floatlist(line):
    return [float(elem) for elem in line.split()]


def read_distance_matrix(reader, n):
    distances = {}
    for ix in range(n):
        row = convert_to_floatlist(next(reader))
        for jy in range(ix+1, n):
            distances[(ix, jy)] = row[jy]
    return DistanceMatrix(distances)


def run_hierarchical_clustering(distances, n):
    id_generator = count(n)
    clusters = {k: Cluster(id=k, content=(k,)) for k in range(n)}
    new_clusters = []
    while len(clusters) > 1:
        id_1, id_2 = distances.closest_clusters()
        cluster_1 = clusters.pop(id_1)
        cluster_2 = clusters.pop(id_2)
        next_id = next(id_generator)
        next_cluster = Cluster(id=next_id, content=cluster_1.content+cluster_2.content)
        clusters[next_id] = next_cluster
        new_clusters.append(next_cluster)
        distances.update(cluster_1, cluster_2, next_id)
    return new_clusters


def main():
    reader = sys.stdin
    n = int(next(reader))
    distances = read_distance_matrix(reader, n)
    result = run_hierarchical_clustering(distances, n)
    for cluster in result:
        print(' '.join(map(lambda x: str(x + 1), cluster.content)))


if __name__ == '__main__':
    main()
