# Implement the Lloyd Algorithm for k-Means Clustering
import functools as ft
import math
import sys


def convert_to_numbers(line, converter_func):
    return [converter_func(elem) for elem in line.split()]


def read_points(reader):
    return [convert_to_numbers(line, float) for line in reader]


def euclidean_distance(p, q):
    return math.sqrt(sum((p_i - q_i)**2 for p_i, q_i in zip(p, q)))


def select_closest_center(point, centers):
    calc_distance_from_point = ft.partial(euclidean_distance, q=point)
    distances = map(calc_distance_from_point, centers)
    min_ix, _ = min(enumerate(distances), key=lambda x: x[1])
    return min_ix


def assign_points_to_centers(points, centers):
    return [select_closest_center(point, centers) for point in points]


def recalculate_centers(points, center_assignment, k):
    centers = []
    for cluster_id in range(k):
        cluster_points = [point for point, ix in zip(points, center_assignment) if ix == cluster_id]
        cluster_size = len(cluster_points)
        center = [sum(coordinates) / cluster_size for coordinates in zip(*cluster_points)]
        centers.append(center)
    return centers


def are_same_centers(centers, updated_centers):
    return all(euclidean_distance(center, updated_center) <= 1e-12
               for center, updated_center in zip(centers, updated_centers))


def run_k_means_clustering(points, k):
    centers = points[:k]
    while True:
        center_assignment = assign_points_to_centers(points, centers)
        updated_centers = recalculate_centers(points, center_assignment, k)
        if are_same_centers(centers, updated_centers):
            return updated_centers

        centers = updated_centers


def main():
    reader = sys.stdin
    k, _ = convert_to_numbers(next(reader), int)
    points = read_points(reader)
    result = run_k_means_clustering(points, k)
    for point in result:
        print(' '.join(map(str, point)))


if __name__ == '__main__':
    main()
