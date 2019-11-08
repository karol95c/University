import functools

def memoize(func):
    cache = func.cache = {}
    @functools.wraps(func)
    def memoized_func(*args, **kwargs):
        key = str(args) + str(kwargs)
        if key not in cache:
            cache[key] = func(*args, **kwargs)
        return cache[key]
    return memoized_func

@memoize
def sudan(n, x, y):
    if n == 0:
        return x + y
    if y == 0:
        return x
    else:
        return sudan(n - 1, sudan(n, x, y - 1), sudan(n, x, y - 1) + y)
    
print (sudan(2, 3, 1))
print (sudan(2, 4, 1))
print (sudan(2, 1, 2))
print (sudan(2, 2, 2))