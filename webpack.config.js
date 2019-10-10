const path = require('path')
const Dotenv = require('dotenv-webpack')

module.exports = {
  entry: './src/main.js',
  output: {
    filename: 'main.js',
    path: path.resolve(__dirname, 'public'),
  },
  plugins: [
    new Dotenv(),
  ],
  mode: 'development',
}
