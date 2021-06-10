# Compute the Squared Error Distortion
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


def calc_distortion(points, centers):
    n = len(points)
    return sum(distance_from_centers(point, centers) ** 2 for point in points) / n


def main():
    reader = sys.stdin
    k, _ = convert_to_numbers(next(reader), int)
    centers = [convert_to_numbers(next(reader), float) for _ in range(k)]
    _ = next(reader)
    points = read_points(reader)
    result = calc_distortion(points, centers)
    print(result)


if __name__ == '__main__':
    main()
