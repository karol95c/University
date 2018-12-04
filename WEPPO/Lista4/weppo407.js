var ipMap = new Map();
var arr = [['', 0], ['', 0], ['', 0]];

function ipRapport()
{
    processFile('./WEPPO/Lista4/logs.txt');
    
}

var value = 0;
function setCount(ip)
{
    value = ipMap.get(ip);
    var f = 0;
    if( value === undefined)
    {
        ipMap.set(ip, 1);
    }
    else ipMap.set(ip, value + 1);

}

function checkBiggestThree()
{
    for (const [k, v] of ipMap.entries()) 
    {
        if (v > arr[2][1] )
        {
            arr[2] = [k, v];
        }
        arr.sort(compareMapElements);
    }

    for (x of arr)
    {
        console.log(x[0] + " " + x[1]);
    }
}

function compareMapElements(a, b) {
    return a[1] < b[1];
}

function processFile(inputFile) {

    var temp;
    var lineReader = require('readline').createInterface({
            input: require('fs').createReadStream(inputFile)
        });
      
        lineReader.on('line', function (line) {
            temp = line.split(' ');
            setCount(temp[1].toString());
        });
        lineReader.on('close', function () {
            checkBiggestThree();
            });
        }


ipRapport();