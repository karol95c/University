function fibRec(n)
{
    if (n > 1) return fibRec(n - 1) + fibRec(n - 2);
    else return n;
}
// console.log(fibRec(11));

function fibIter(n)
{
    var a = 0;
    var b = 1;
    var temp;
    for(i = 0; i < n; i++)
    {
        temp = b;
        b += a;
        a = temp;
    }
    return temp;
}
// console.log(fibIter(11));
const {performance} = require('perf_hooks');
var timeEnd;
var output = [];
function countTime()
{
    var n = 1;
    while(n > 0)
    {
        var recT0 = performance.now();
        fibRec(n);
        var recT1 = performance.now();
        var iterT0 = performance.now();
        fibIter(n);
        var iterT1 = performance.now();
        console.log(n + ": " + (recT1- recT0) + " " + (iterT1 - iterT0));
        ++n;
        if (recT1 - recT0 > 5000) n = -1;
    }
}

countTime();