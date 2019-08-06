var foo = {
    i : 199,
    a : 1,
    b : 1,
    get bar()
    {
        return foo.i;
    },
    set bar(i) 
    {
        foo.i = i;
    }
}

Object.defineProperty(foo, 'x', {
    value: 1,
});

Object.defineProperty(foo, 'c', { 
    set: function(y) { this.b = y; },
    get: function() { return this.b } });


Object.defineProperty( foo, 'qux', {
    get : function() {
        return 17;
    }
});

Object.defineProperty( foo, 'baz', {
    value : function() {
        return 34;
    }
});

console.log(foo.x);
console.log(foo.a);
console.log(foo.c);
foo.c = 10;
console.log(foo.b);
console.log(foo.qux);
console.log(foo.baz());