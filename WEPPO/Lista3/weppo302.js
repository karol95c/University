const {performance} = require('perf_hooks');
function fib(n)
{
    if (n > 1) return fib(n - 1) + fib(n - 2);
    else return n;
}

function memoize(fn)
{
    var cache = {};

    return function(n)
    {
        if (n in cache) return cache[n];
        else
        {
            var result = fn(n);
            cache[n] = result;
            return result;
        }
    }
}

function countTime()
{
    var n = 1;
    var memoFib = memoize(fib);
    while(n > 0)
    {
        var recT0 = performance.now();
        memoFib(n);
        var recT1 = performance.now();
        console.log(n + ": " + (recT1- recT0));
        ++n;
        if (recT1 - recT0 > 5000) n = -1;
    }
}

countTime();
