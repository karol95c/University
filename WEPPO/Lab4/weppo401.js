//Zadanie 1 - konstruktor

class Tree
{
    constructor(data, leftSub = null, rightSub = null) 
    { 
        this.data = data; 
        this.left = leftSub; 
        this.right = rightSub;
    }

    // *[Symbol.iterator] () {
    //     if (this.right == null && this.left == null) yield this.data;
    //     yield* this.right;
    //     yield this.data;
    //     yield* this.right;
    // }
    * [Symbol.iterator]() {

        if (this.left) {
            yield* this.left;
        }
        yield this.data;
        if (this.right) {
            yield* this.right;
        }
    }
    
}


var subTree1 = new Tree(new Tree(10));
var subTree2 = new Tree(20, subTree1, null);
// var root = new Tree(30, subTree2, null);
var root = new Tree(30, new Tree(20, new Tree(10), null), null);

// Zadanie 2 - iterator


// enumeracja wartości z węzłów
for ( var e of root )
{
    console.log( e );
}