module.exports = {
    binary: '../../.cabal-sandbox/bin/digital_impro',
    sequelize: {
        dialect: 'sqlite',
        storage: '../data/database.sqlite',
        operatorsAliases: false,
    }
}