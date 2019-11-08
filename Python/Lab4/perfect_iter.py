from math import sqrt, floor, ceil
import time
PERFECT_LENGTH = 10000
TEST_VALUES = [10, 100, 1000, 10000]

def perfect_numbers(n):
    perfect = []
    for i in range (3, n +1):
        sum = 0;
        for j in range(1, ceil(i/2) + 1):
            if i % j == 0:
                sum += j;
        if (sum == i):
            perfect.append(i)
    
    return perfect

def test_imperative(n):
    start = time.time()
    perfect_iterative = perfect_numbers(n)
    end = time.time()
    return end - start

def perfect_comprehensive(n):
    return [number for number in range (3, n)
        if sum(div for div in range (1, ceil(number/2) + 1) if number % div == 0) == number]

def test_comprehensive(n):
    start = time.time()
    perfect_comprehensive = perfect_numbers(n)
    end = time.time()
    return end - start

def perfect_functional(n):
    return list (filter (lambda x : sum(div for div in range (1, ceil(x/2) + 1) if x % div == 0) == x))

def test_functional(n):
    start = time.time()
    perfect_functional = perfect_numbers(n)
    end = time.time()
    return end - start

def perfect_iterator(n):
    if (n < 2): return []
    i = 2
    while (i <= n):
        if sum(div for div in range (1, ceil(i/2) + 1) if i % div == 0) == i:
            yield i
        i += 1


def test_iterator(n):
    start = time.time()
    a = perfect_iterator(n)
    end = time.time()
    return end - start

def test_functions():
    JUST = 16
    print(''.ljust(6), "| imperative".ljust(JUST), "| functional".ljust(JUST), "| comprehensive ".ljust(JUST), "| iterator".ljust(JUST))
    
    for value in TEST_VALUES:
        test0 = ("| " + "{:3.7f}".format(test_imperative(value))).ljust(JUST)
        test1 = ("| " + "{:3.7f}".format(test_functional(value))).ljust(JUST)
        test2 = ("| " + "{:3.7f}".format(test_comprehensive(value))).ljust(JUST)
        test3 = ("| " + "{:3.7f}".format(test_iterator(value))).ljust(JUST)

        print(str(value).rjust(6), test0, test1, test2, test3)

test_functions()