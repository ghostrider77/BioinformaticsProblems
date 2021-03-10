# Solve the Turnpike Problem
import sys

from collections import Counter


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def read_positive_differences(line):
    diffs = convert_to_intlist(line)
    return Counter(filter(lambda x: x > 0, diffs))


def calc_positive_differences(xs, elem):
    return Counter([abs(elem - x) for x in xs])


def is_subset(delta, diffs):
    return all(diffs.get(item, 0) >= count for item, count in delta.items())


def remove(differences, items):
    for item, count in items.items():
        current_count = differences.get(item, 0)
        if count == current_count:
            _ = differences.pop(item, None)
        else:
            differences.subtract({item: count})


def insert_largest_value(xs, differences, y, m):
    delta = calc_positive_differences(xs, y)
    if is_subset(delta, differences):
        xs.append(y)
        remove(differences, delta)
        result = solve(xs, differences, m)
        if result is not None:
            return result
        _ = xs.pop(-1)
        differences.update(delta)
    return None


def solve(xs, differences, m):
    if not differences:
        return tuple(xs)

    y = max(differences)
    result = insert_largest_value(xs, differences, y, m)
    if result is not None:
        return result

    result = insert_largest_value(xs, differences, m - y, m)
    if result is not None:
        return result

    return None


def solve_turnpike_problem(differences):
    m = max(differences)
    remove(differences, {m: 1})
    distances = [0, m]
    result = solve(distances, differences, m)
    return sorted(result)


def main():
    data = sys.stdin.read().splitlines()
    differences = read_positive_differences(data[0])
    result = solve_turnpike_problem(differences)
    print(' '.join(map(str, result)))


if __name__ == '__main__':
    main()
