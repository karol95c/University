function Foo(suma) {
    this.s = suma;
}
Foo.prototype.Bar = function()
{
    console.log("This is Bar");
    var Qux = (function()
    {
        console.log("This is Qux");
    });
    Qux();
    // var Qux2 = function()
    // {
    //     console.log("This is Qux2");
    // };
    // Qux2();
};
var foo = new Foo();
// foo.Bar();
foo.Bar();