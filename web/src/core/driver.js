const { spawn } = require('child_process');
const { EOL } = require('os');
const config = require('../config');

const { join } = require('path');

const binary = join(__dirname, '..', config.binary);


module.exports = () => {
    const bin = spawn(binary, [], {

    });
    bin.stdin.setEncoding('utf-8');
    bin.stdout.setEncoding('utf-8');

    bin.stdin.write(JSON.stringify({ event: 'generate' }) + '\n');

    let response = '';

    let charsc = 0;


    bin.stdout.on('data', (chunk) => {
        //response += chunk.toString();
        
        //console.log(chunk.toString().length);
        console.log(chunk);
        //charsc += chunk.toString().length;
        //console.log(charsc);
        //return true;
        /*if(chunk.toString().endsWith('\n')) {
            console.log('endswith');
            console.log(response);
        }*/
    });

    bin.stderr.on('data', (chunk) => {
        console.log('err', chunk);
    })
}