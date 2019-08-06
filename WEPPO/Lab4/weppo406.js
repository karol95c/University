const fs = require('fs');
// fs.setE

fs.readFile('./WEPPO/Lab4/lab6a.txt', 'utf-8', function read(err, data) {
    if (err) {
        throw err;
    }
    content = data;

    processFile();
});

function processFile() {
    console.log(content);
}
