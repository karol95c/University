function fibRec(n)
{
    if (n > 1) return fibRec(n - 1) + fibRec(n - 2);
    else return n;
}
console.log(fibRec(11));

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
console.log(fibIter(11));