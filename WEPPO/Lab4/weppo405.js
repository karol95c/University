process.stdin.setEncoding('utf8');

process.stdout.write('Wpisz swoje imię i nazwisko, następnie wciśnij ENTER \n');

process.stdin.on('readable', () => {
    const fullName = process.stdin.read();
    if (fullName !== null) {
        process.stdout.write('Witaj ' + fullName);
    }
});

// process.stdin.on('end', () => {
//     process.stdout.write('end');
// });