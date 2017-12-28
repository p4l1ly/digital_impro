const { join } = require('path');

const PUBLIC_PATH = '../public';

const j = (p) => join(__dirname, PUBLIC_PATH, p);
const mainPage = j('index.html');

module.exports = function routes() {

    this.get('/',function(req,res){
        res.sendFile(mainPage);
    });

    this.get('/preview/:id', function(req, res){
        res.sendFile(mainPage);
    });

}