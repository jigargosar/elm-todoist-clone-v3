const HtmlWebpackPlugin = require('html-webpack-plugin')

module.exports = {
  plugins: [new HtmlWebpackPlugin({ template: 'src/index.html' })],

  devServer: {
    contentBase: 'public',
    watchContentBase: true,
  },
}
