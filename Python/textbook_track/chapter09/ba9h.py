# Pattern Matching with the Suffix Array
import itertools as it
import sys

from collections import Counter, defaultdict


def read_patterns(reader):
    return list(map(lambda s: s.rstrip(), reader))


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


def compute_suffix_array(text):
    n = len(text)
    order = sort_single_characters(text)
    classes = compute_character_classes(text, order, n)
    cyclic_shift_size = 1
    while cyclic_shift_size < n:
        order = sort_doubled_shifts(order, classes, n, cyclic_shift_size)
        classes = update_classes(order, classes, cyclic_shift_size, n)
        cyclic_shift_size *= 2
    return order


def pattern_matching_with_suffix_array(suffix_array, text, n, pattern):
    min_index = 0
    max_index = n
    while min_index < max_index:
        middle_index = (min_index + max_index) // 2
        if pattern > text[suffix_array[middle_index]:]:
            min_index = middle_index + 1
        else:
            max_index = middle_index
    first = min_index

    max_index = n
    while min_index < max_index:
        middle_index = (min_index + max_index) // 2
        suffix = text[suffix_array[middle_index]:]
        if suffix.startswith(pattern):
            min_index = middle_index + 1
        elif pattern < suffix:
            max_index = middle_index
        else:
            min_index = middle_index + 1
    last = max_index

    if first > last:
        return []

    return [suffix_array[index] for index in range(first, last)]


def multiple_pattern_matching(text, patterns):
    suffix_array = compute_suffix_array(text)
    n = len(text)
    matched_indices = set()
    for pattern in patterns:
        indices = pattern_matching_with_suffix_array(suffix_array, text, n, pattern)
        matched_indices.update(indices)
    return matched_indices


def main():
    reader = sys.stdin
    text = next(reader).rstrip() + '$'
    patterns = read_patterns(reader)
    result = multiple_pattern_matching(text, patterns)
    print(' '.join(map(str, result)))


if __name__ == "__main__":
    main()
