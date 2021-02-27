# Find the Minimum Number of Coins Needed to Make Change
import sys


def convert_to_intlist(line):
    return [int(item) for item in line.split(',')]


def calc_minimum_number_of_coins(amount, coins):
    changes = [0] * (amount+1)
    for money in range(1, amount+1):
        changes[money] = 1 + min(changes[k] for coin in coins if (k := money - coin) >= 0)
    return changes[amount]


def main():
    data = sys.stdin.read().splitlines()
    money = int(data[0])
    coins = convert_to_intlist(data[1])
    result = calc_minimum_number_of_coins(money, coins)
    print(result)


if __name__ == '__main__':
    main()
