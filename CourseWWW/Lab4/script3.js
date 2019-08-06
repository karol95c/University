function addSquare(width, height)
{
    var node = document.createElement("div");
    myBox = document.getElementById("mybox");
    myBox.appendChild(node);
    node.className = 'square2';
    node.style.left = width.toString() + "%";
    node.style.top = height.toString() + "%";
    myBox = document.getElementById("mybox");
    myBox.appendChild(node);
}

function getRandomArbitrary(min, max) {
    return Math.random() * (max - min) + min;
}


function addRandomSquares()
{
    addRandomQuarter(60, 95, 7, 35);
    addRandomQuarter(5, 40, 7, 35);
    addRandomQuarter(5, 40, 65, 93);
    addRandomQuarter(60, 95, 65, 93);
}
var gamePlayed = 0;
function addRandomQuarter(minWidth, maxWidth, minHeight, maxHeight)
{
    var quarterHeights = [];
    var quarterWidths = [];
    function notCollideHeight(y)
    {
        return Math.abs(y - height) >= 6;
    }

    function notCollideWidth(x)
    {
        return Math.abs(x - width) >= 8;
    }
    for (var i = 0; i < 2; ++i)
    {
        var width = getRandomArbitrary(minWidth, maxWidth);
        var height = getRandomArbitrary(minHeight, maxHeight);
        quarterWidths[i] = width;
        quarterHeights[i] = height;
        while (!quarterHeights.every(notCollideHeight))
        {
            height = getRandomArbitrary(minHeight, maxHeight);
        }
    
        while (!quarterWidths.every(notCollideWidth))
        {
            width = getRandomArbitrary(minWidth, maxWidth);
        }
        quarterWidths[i] = width;
        quarterHeights[i] = height;
        addSquare(width, height);
    }
}

function GameHandle()
{
    addRandomSquares();
    var gameOff = true;
    var counter = 0;

    var blueChecked = false;
    var square2CN = document.getElementsByClassName('square2');
    var results = document.getElementById("results");
    for (var i =0; i<square2CN.length; i++)
    {
        square2CN[i].addEventListener('mouseover', function()
        {
            
            if (this.style.backgroundColor == "red")
            {
                c+=10;
            }
            else if(blueChecked == true)
            {

                this.style.backgroundColor = "red";
                counter++;
                blueChecked = false;
            }
            if (counter == 8){
                gamePlayed++;
                results.innerHTML += "<br/>" + gamePlayed.toString()+ ". " + c + " sekund";
                c = 0;
                gameOff = true;
                startNewGame();
                counter = 0;

            }
        }, false);
    }
    var mainSquare = document.getElementById("square");
    mainSquare.addEventListener("mouseover", function(){
        if (gameOff && counter == 0)
        {
            gameOff = false;
        }
        if(!blueChecked)
        {
            stopCount();
            blueChecked = true;
        }
    });
    mainSquare.addEventListener("mouseleave", function(){
        if(blueChecked && !gameOff)
        {
            startCount();
            blueChecked = true;
        }
    });
}

var c = 0;
var t;
var timer_is_on = 0;

function timedCount() {
    document.getElementById("timer").innerText = "Time passed: " + c;
    c = c + 1;
    t = setTimeout(timedCount, 1000);
}

function startCount() {
    if (!timer_is_on) {
        timer_is_on = 1;
        timedCount();
    }
}

function stopCount() {
    clearTimeout(t);
    timer_is_on = 0;
}

function removeElementsByClass(className){
    var elements = document.getElementsByClassName(className);
    while(elements.length > 0){
        elements[0].parentNode.removeChild(elements[0]);
    }
}

function startNewGame()
{

    removeElementsByClass('square2');
    GameHandle();
}

startNewGame();