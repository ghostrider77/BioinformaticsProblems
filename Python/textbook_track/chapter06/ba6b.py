# Compute the Number of Breakpoints in a Permutation
import sys

from enum import Enum


class Sign(Enum):
    PLUS = '+'
    MINUS = '-'


class SignedNumber:
    def __init__(self, sign, number):
        self._sign = sign
        self._number = number
        self._value = number if sign == Sign.PLUS else -number

    def __repr__(self):
        return f'{self._sign.value}{self._number}'

    def __eq__(self, that):
        return self.value == that.value

    def __add__(self, that):
        result = self.value + that.value
        if result < 0:
            return SignedNumber(Sign.MINUS, abs(result))

        return SignedNumber(Sign.PLUS, result)

    def __neg__(self):
        opposite_sign = Sign.PLUS if self.sign == Sign.MINUS else Sign.MINUS
        return SignedNumber(opposite_sign, self.number)

    def __sub__(self, that):
        return self + (-that)

    @property
    def sign(self):
        return self._sign

    @property
    def number(self):
        return self._number

    @property
    def value(self):
        return self._value


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


def create_extended_permutation(permutation):
    extended_permutation = [SignedNumber(Sign.PLUS, 0)]
    for item in permutation:
        extended_permutation.append(item)

    extended_permutation.append(SignedNumber(Sign.PLUS, len(permutation) + 1))
    return SignedPermutation(extended_permutation)


def calc_nr_breakpoints(permutation):
    extended_permutation = create_extended_permutation(permutation)
    one = SignedNumber(Sign.PLUS, 1)
    nr_breakpoints = 0
    for a, b in zip(extended_permutation, extended_permutation[1:]):
        if b - a != one:
            nr_breakpoints += 1
    return nr_breakpoints


def main():
    reader = sys.stdin
    permutation = read_permutation(next(reader))
    result = calc_nr_breakpoints(permutation)
    print(result)


if __name__ == '__main__':
    main()
