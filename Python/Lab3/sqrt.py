from math import sqrt, floor
from random import randint

def my_sqrt(n):
    sum = 0
    i = 1
    while (True):
        sum += (2 * i - 1)
        if (sum > n):
            return i - 1
        i += 1

def test(n):
    print (floor(sqrt(n)))
    print (my_sqrt(n))
    return floor(sqrt (n)) == my_sqrt(n)

def tests(k):
    for i in range(k):
        random = randint(1, 2492494)
        print ("Test for {} :  {} \n", random, test(random))

tests(5)