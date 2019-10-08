const HtmlWebpackPlugin = require('html-webpack-plugin')

module.exports = {
  resolve: {
    extensions: ['.js', '.elm'],
  },
  plugins: [new HtmlWebpackPlugin({ template: 'src/index.html' })],
  module: {
    rules: [
      {
        include: /\.elm/,
        use: ['elm-hot-webpack-loader', 'elm-webpack-loader'],
      },
      { include: /\.css/, use: ['style-loader', 'css-loader'] },
    ],
  },
  devServer: {
    hot: true,
    overlay: true,
  },
}