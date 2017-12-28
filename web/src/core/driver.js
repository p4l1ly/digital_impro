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


    bin.stdout.on('data', (chunk) => {
        response += chunk;
    });

    bin.stderr.on('data', (chunk) => {
        console.log('err', chunk);
    })
}