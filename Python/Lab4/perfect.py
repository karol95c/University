from math import sqrt, floor, ceil
import time
PERFECT_LENGTH = 10000

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

start = time.time()
perfect_iterative = perfect_numbers(PERFECT_LENGTH)
end = time.time()
print ("Time elapsed for perfect: " +str (end - start))
print (perfect_iterative)
print (type(perfect_iterative) == list)
print (30 * '-')

def perfect_comprehensive(n):
    return [number for number in range (3, n)
        if sum(div for div in range (1, ceil(number/2) + 1) if number % div == 0) == number]

start = time.time()
perfect_comprehensive = perfect_numbers(PERFECT_LENGTH)
end = time.time()
print ("Time elapsed for comprehensive: " + str(end - start))
print (perfect_comprehensive)
print (type(perfect_comprehensive) == list)
print (30 * '-')


def perfect_functional(n):
    return list (filter (lambda x : sum(div for div in range (1, ceil(x/2) + 1) if x % div == 0) == x))


start = time.time()
perfect_functional = perfect_numbers(PERFECT_LENGTH)
end = time.time()
print ("Time elapsed for functional: " + str(end - start))
print (perfect_functional)
print (type(perfect_functional) == list)
print (30 * '-')