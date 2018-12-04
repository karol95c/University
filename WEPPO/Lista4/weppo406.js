const fs = require('fs');
// fs.setE

fs.readFile('./WEPPO/Lista4/zad6a.txt', 'utf-8', function read(err, data) {
    if (err) {
        throw err;
    }
    content = data;

    processFile();
});

function processFile() {
    console.log(content);
}
