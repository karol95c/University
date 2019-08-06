function map(a, f)
{
    var temp = [];
    for (i in a)
    {
        temp.push(f(a[i]));
    }
    return temp;
}

function forEach(a, f)
{
    for (i in a)
    {
        f(a[i]);
    }
}

function filter(a, f)
{
    var temp = [];
    for (i in a)
    {
        if (f(a[i])) temp.push(a[i]);
    }
    return temp;
}

var a = [1,2,3,4, 5, 6, 7, 8];
forEach( a, _ => { console.log( _ ); } );
// [1,2,3,4]
console.log(filter( a, _ => _ < 3 ));
console.log(filter(a, range));
// [1,2]
console.log(map( a, _ => _ * 2 ));
console.log(map( a, cube));
// [2,4,6,8]

function range(x)
{
    return x > 3 && x < 7;
}

function cube(x)
{
    return x * x * x;
}
