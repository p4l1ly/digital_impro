const express = require('express');
const { join } = require('path');

const app = express();

app.use('/js', express.static(join(__dirname, '../public/js/libs')));
app.use('/js/app.js', express.static(join(__dirname, '../public/js/bundle.js')));
app.use('/favicon.ico', express.static(join(__dirname, '../public/favicon.ico')));

require('./routes').call(app);
require('./sequelize')(require('./config'));


var server = require('http').createServer(app);
var io = require('socket.io')(server);

require('./sockets')(io);

const listener = server.listen(process.env.PORT || 8080, () => {
    console.log(`Servers listening on http://localhost:${listener.address().port}`);
});