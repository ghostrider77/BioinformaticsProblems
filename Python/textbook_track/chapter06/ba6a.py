# Find the Minimum Number of Coins Needed to Make Change
import itertools as it
import sys

from enum import Enum


class Sign(Enum):
    PLUS = '+'
    MINUS = '-'


class SignedNumber:
    def __init__(self, sign, number):
        self._sign = sign
        self._number = number

    def __repr__(self):
        return f'{self._sign.value}{self._number}'

    @property
    def sign(self):
        return self._sign

    @property
    def number(self):
        return self._number


class SignedPermutation:
    def __init__(self, permutation):
        self._permutation = tuple(permutation)

    def __repr__(self):
        return f"({' '.join(map(str, self._permutation))})"

    def __len__(self):
        return len(self._permutation)

    def __getitem__(self, k):
        return self._permutation[k]

    def __iter__(self):
        yield from self._permutation


def read_permutation(line):
    line = line.replace('(', '').replace(')', '')
    permutation = []
    for item in line.split():
        signed_number = SignedNumber(Sign(item[0]), int(item[1:]))
        permutation.append(signed_number)
    return SignedPermutation(permutation)


def find_k_in_permutation_suffix(permutation, k):
    for ix, item in enumerate(permutation[k:], start=k):
        if item.number == k + 1:
            return ix
    return None


def perform_k_reversal(permutation, k):
    index = find_k_in_permutation_suffix(permutation, k)
    k_reversal = []
    for ix, item in enumerate(it.chain(permutation[:k], reversed(permutation[k:index+1]), permutation[index+1:])):
        if k <= ix <= index:
            signed_number = opposite_sign(item)
        else:
            signed_number = item
        k_reversal.append(signed_number)

    return SignedPermutation(k_reversal)


def is_k_sorted(permutation, k):
    signed_number = permutation[k]
    return signed_number.sign == Sign.PLUS and signed_number.number == k + 1


def has_negative_sign(permutation, k):
    return permutation[k].sign == Sign.MINUS


def opposite_sign(signed_number):
    if signed_number.sign == Sign.PLUS:
        return SignedNumber(Sign.MINUS, signed_number.number)

    return SignedNumber(Sign.PLUS, signed_number.number)


def change_sign(permutation, k):
    updated_permutation = [opposite_sign(item) if ix == k else item for ix, item in enumerate(permutation)]
    return SignedPermutation(updated_permutation)


def greedy_sorting(permutation):
    n = len(permutation)
    reversals = []
    for k in range(n):
        if not is_k_sorted(permutation, k):
            permutation = perform_k_reversal(permutation, k)
            reversals.append(permutation)
            if has_negative_sign(permutation, k):
                permutation = change_sign(permutation, k)
                reversals.append(permutation)
    return reversals


def main():
    reader = sys.stdin
    permutation = read_permutation(next(reader))
    result = greedy_sorting(permutation)
    for reversal in result:
        print(reversal)


if __name__ == '__main__':
    main()
