// function fib() {

//     var a = 1, b = 1; current = 0;
//     return {
//         next : function() {
//             current = a;
//             a = b;
//             b += current;
//             return {
//                 value : current,
//                  done : current === Infinity
//             }
//         }
//     }
// }

function* fib (current = 1, next = 1) {
    if (current === Infinity) {
      return 0;
    }
  
    yield current;
    yield* fib(next, current + next);
}

var _it = fib();
// for ( var _result; _result = _it.next(), !_result.done; )
// {
//     console.log( _result.value );
// }

// for ( var i of fib(10) ) {
//     console.log( i );
//     }


function* take(it, top) {
    if (top == 0) return 0;
    
    yield it.next().value;
    yield* take(it, --top);
}

for (let num of take( fib(), 10 ) ) {
    console.log(num);
}