const path = require('path')
const Dotenv = require('dotenv-webpack')

module.exports = {
  entry: './src/main.js',
  output: {
    filename: 'main.js',
    path: path.resolve(__dirname, 'public'),
  },
  module: {
    rules: [{
      test: /\.elm$/,
      exclude: [/elm-stuff/, /node_modules/],
      use: {
        loader: 'elm-webpack-loader',
        options: {},
      },
    }],
  },
  plugins: [
    new Dotenv(),
  ],
}
