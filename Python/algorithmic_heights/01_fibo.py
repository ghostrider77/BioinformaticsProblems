# Fibonacci Numbers
import sys


def fibonacci(n):
    k = 1
    a, b = 0, 1
    while k <= n:
        a, b = b, a + b
        k += 1
    return a


def main():
    reader = sys.stdin
    n = int(next(reader))
    result = fibonacci(n)
    print(result)


if __name__ == '__main__':
    main()
