var webpack = require('webpack');

var BUILD_ENV = process.env.BUILD_ENV || 'development';

module.exports = {
  entry: './app/index.js',
  output: {
    path: 'app',
    publicPath: 'app',
    filename: 'script.js',
  },
  module: {
    noParse: [/\.elm$/],
    loaders: [
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader: 'elm-webpack-loader?cwd=' + __dirname,
      },
    ],
  },
  plugins: [
  ].concat(
    BUILD_ENV === 'production'
    ? [
        new webpack.optimize.UglifyJsPlugin({
          test: /\.js$/,
          sourceMap: false,
          compress: {
            warnings: false,
            screw_ie8: true,
          },
        }),
      ]
    : []
  ),
};
