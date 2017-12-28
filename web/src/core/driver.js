const { spawn } = require('child_process');

const config = require('../config');

const { join } = require('path');

const binary = join(__dirname, '..', config.binary);


module.exports = () => {
    const bin = spawn(binary);


    bin.stdin.write(JSON.stringify({ event: 'generate' }) + '\n');

    bin.stdout.on('data', (chunk) => {
        console.log(JSON.parse(chunk.toString()));
    });

}