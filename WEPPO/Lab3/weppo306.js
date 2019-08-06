function createGeneratorFunction(n)
{
    return function createGenerator() {
    var _state = 0;
    return {
        next : function() {
            return {
                value : _state,
                done : _state++ >= n
            }
            }
        }
    }
}


function* idMaker() {
    var index = 0;
    while(true)
      yield index++;
  }

var foo = {
    [Symbol.iterator] : createGeneratorFunction(14)
};

for ( var f of foo )
    console.log(f);