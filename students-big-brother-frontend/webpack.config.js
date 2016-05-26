const path = require('path')
const webpack = require('webpack')
const CopyWebpackPlugin = require('copy-webpack-plugin');
const production = process.env.NODE_ENV === 'production'

let plugins = []

// if (production) {
    plugins = plugins.concat([
      new webpack.optimize.UglifyJsPlugin({
          mangle:   true,
          compress: {
              warnings: false, // Suppress uglification warnings
          },
      }),
      new CopyWebpackPlugin([
          { from: 'src/index.html', to: 'index.html' },
          { from: 'src/admin.html', to: 'admin.html' }
      ])
    ]);
// }

module.exports = {
  entry: {
    index: ['babel-polyfill','./src/index.js'],
    admin: ['babel-polyfill','./src/admin.js'],
  },
  output: {
    path: "./dist",
    publicPath: "/",
    filename: "[name].bundle.js"
  },
  debug:   !production,
  devtool: production ? false : 'eval',
  module: {
    loaders: [
      { test: /\.css$/, loaders: ['style', 'css'] },
      { test: /\.json$/, loader: 'raw-loader' },
      { test:   /\.(png|gif|jpe?g|svg)$/, loader: 'url'},
      {
        test: /\.js$/,
        include: path.join(__dirname, 'src'),
        loader:  'babel',
        query: {
          presets: ['es2015', 'stage-0']
        }
      },
    ]
  },
  plugins
};
