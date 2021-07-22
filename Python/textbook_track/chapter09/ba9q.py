# Construct the Partial Suffix Array of a String
import itertools as it
import sys

from collections import Counter, defaultdict


def sort_single_characters(text):
    indices = defaultdict(list)
    for ix, char in enumerate(text):
        indices[char].append(ix)

    unique_chars = sorted(indices.keys())
    char_indices = (indices[char] for char in unique_chars)
    return tuple(it.chain.from_iterable(char_indices))


def compute_character_classes(text, order, n):
    classes = [0] * n
    label = 0
    classes[order[0]] = label
    for o1, o2 in zip(order, order[1:]):
        if text[o1] != text[o2]:
            label += 1
        classes[o2] = label
    return tuple(classes)


def count_cumulative_class_sizes(classes, n):
    counts = [0] * n
    class_sizes = Counter(classes)
    for class_id, class_size in class_sizes.items():
        counts[class_id] = class_size

    for ix in range(1, n):
        counts[ix] += counts[ix-1]

    return counts


def sort_doubled_shifts(order, classes, n, cyclic_shift_size):
    counts = count_cumulative_class_sizes(classes, n)
    new_order = [0] * n
    for ix in range(n-1, -1, -1):
        start = (order[ix] - cyclic_shift_size) % n
        class_id = classes[start]
        counts[class_id] -= 1
        new_order[counts[class_id]] = start
    return tuple(new_order)


def update_classes(order, classes, cyclic_shift_size, n):
    new_classes = [0] * n
    label = 0
    new_classes[order[0]] = label
    for current, previous in zip(order[1:], order):
        mid = (current + cyclic_shift_size) % n
        mid_previous = (previous + cyclic_shift_size) % n
        if classes[current] != classes[previous] or classes[mid] != classes[mid_previous]:
            label += 1
        new_classes[current] = label
    return tuple(new_classes)


def compute_full_suffix_array(text):
    n = len(text)
    order = sort_single_characters(text)
    classes = compute_character_classes(text, order, n)
    cyclic_shift_size = 1
    while cyclic_shift_size < n:
        order = sort_doubled_shifts(order, classes, n, cyclic_shift_size)
        classes = update_classes(order, classes, cyclic_shift_size, n)
        cyclic_shift_size *= 2
    return order


def compute_partial_suffix_array(text, k):
    suffix_array = compute_full_suffix_array(text)
    return list(filter(lambda x: x[1] % k == 0, enumerate(suffix_array)))


def main():
    reader = sys.stdin
    text = next(reader).rstrip()
    k = int(next(reader))
    result = compute_partial_suffix_array(text, k)
    for ix, suffix_ix in result:
        print(f'{ix},{suffix_ix}')


if __name__ == '__main__':
    main()
