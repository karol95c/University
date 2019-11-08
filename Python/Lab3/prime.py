from math import sqrt, floor, ceil
import time

def is_prime(i, n):
    end = floor(sqrt(i)) + 1
    for j in range (2, end):
        if (i % j == 0):
            return False

    return True

PRIMES_LENGTH = 100
def eratosthenes_sieve(n):
    primes = list(range(2, n))
    end = floor(sqrt(n))
    for i in range (2, end):
        for prime in primes:
            if prime % i == 0 and prime != i:
                primes.remove(prime)
    return primes

start = time.time()
prime_iterative = eratosthenes_sieve(PRIMES_LENGTH)
end = time.time()
print ("Time elapsed for prime iterative: " +str (end - start))
print (prime_iterative)
print (type(prime_iterative) == list)
print (30 * '-')



def comprehension_prime(n):
    return [number for number in range(2, n)
     if all(number % div != 0 or number == div for div in range(2, floor(sqrt(n))))]



start = time.time()
prime_comprehensive = comprehension_prime(PRIMES_LENGTH)
end = time.time()
print ("Time elapsed for prime comprehensive: " +str (end - start))
print (prime_comprehensive)
print (type(prime_comprehensive) == list)
print(30*'-')

def filter_prime(n):
    primes = list(range(2, n))
    return list(filter(lambda x: is_prime(x, n), primes))


start = time.time()
prime_functional = filter_prime(PRIMES_LENGTH)
end = time.time()
print ("Time elapsed for prime functional: " +str (end - start))
print (prime_functional)
print (type(prime_functional) == list)
print(30*'-')