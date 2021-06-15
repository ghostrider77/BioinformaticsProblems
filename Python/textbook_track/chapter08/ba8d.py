# Implement the Soft k-Means Clustering Algorithm
import math
import sys

NR_ITERATIONS = 100


def convert_to_numbers(line, converter_func):
    return [converter_func(elem) for elem in line.split()]


def read_points(reader):
    return [convert_to_numbers(line, float) for line in reader]


def euclidean_distance(p, q):
    return math.sqrt(sum((p_i - q_i)**2 for p_i, q_i in zip(p, q)))


def assign_points_to_centers(points, centers, beta, k):
    n = len(points)
    responsibility_matrix = [[0.0] * n for _ in range(k)]
    for ix, center in enumerate(centers):
        for jy, point in enumerate(points):
            dist = euclidean_distance(point, center)
            responsibility_matrix[ix][jy] = math.exp(-beta * dist)
    normalize_responsibility_matrix(responsibility_matrix)
    return responsibility_matrix


def normalize_responsibility_matrix(responsibility_matrix):
    column_sums = [sum(column) for column in zip(*responsibility_matrix)]
    for ix, row in enumerate(responsibility_matrix):
        for jy, (elem, column_sum) in enumerate(zip(row, column_sums)):
            responsibility_matrix[ix][jy] = elem / column_sum


def dot_product(v, w):
    return sum(v_i * w_i for v_i, w_i in zip(v, w))


def recalculate_centers(points, responsibility_matrix, k, m):
    centers = [[0.0] * m for _ in range(k)]
    row_sums = [sum(row) for row in responsibility_matrix]

    for jy, data_column in enumerate(zip(*points)):
        for ix, (responsibility_row, row_sum) in enumerate(zip(responsibility_matrix, row_sums)):
            centers[ix][jy] = dot_product(responsibility_row, data_column) / row_sum
    return centers


def run_soft_k_means_clustering(points, k, m, beta):
    centers = points[:k]
    for _ in range(NR_ITERATIONS):
        responsibility_matrix = assign_points_to_centers(points, centers, beta, k)
        centers = recalculate_centers(points, responsibility_matrix, k, m)
    return centers


def main():
    reader = sys.stdin
    k, m = convert_to_numbers(next(reader), int)
    beta = float(next(reader))
    points = read_points(reader)
    result = run_soft_k_means_clustering(points, k, m, beta)
    for point in result:
        print(' '.join(map(str, point)))


if __name__ == '__main__':
    main()
