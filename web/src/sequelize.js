const { join } = require('path');
const Sequelize = require('sequelize');

module.exports = (conf) => {
    //conf.sequelize.storage = join(__dirname, conf.sequelize.storage)

    const sequelize = new Sequelize('digital_impro', null, null, conf.sequelize);
}
