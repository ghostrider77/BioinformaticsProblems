# Implement FarthestFirstTraversal
import functools as ft
import math
import sys


def convert_to_numbers(line, converter_func):
    return [converter_func(elem) for elem in line.split()]


def read_points(reader):
    return [convert_to_numbers(line, float) for line in reader]


def euclidean_distance(p, q):
    return math.sqrt(sum((p_i - q_i)**2 for p_i, q_i in zip(p, q)))


def distance_from_centers(point, centers):
    distance_from_point = ft.partial(euclidean_distance, q=point)
    return min(map(distance_from_point, centers))


def find_index_that_maximizes_distance(points, center_indices):
    centers = [points[ix] for ix in center_indices]
    max_ix = 0
    max_distance = 0.0
    for ix, point in enumerate(points):
        if ix not in center_indices:
            d = distance_from_centers(point, centers)
            if d >= max_distance:
                max_distance = d
                max_ix = ix
    return max_ix


def farthest_first_traversal(points, k):
    center_indices = [0]
    for _ in range(k-1):
        ix = find_index_that_maximizes_distance(points, center_indices)
        center_indices.append(ix)
    return [points[ix] for ix in center_indices]


def main():
    reader = sys.stdin
    k, _ = convert_to_numbers(next(reader), int)
    points = read_points(reader)
    result = farthest_first_traversal(points, k)
    for point in result:
        print(' '.join(map(str, point)))


if __name__ == '__main__':
    main()
