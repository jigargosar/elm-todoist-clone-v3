const HtmlWebpackPlugin = require('html-webpack-plugin')

module.exports = (_, config) => {
  const prod = config.mode === 'production'
  return {
    resolve: {
      extensions: ['.js', '.elm'],
    },
    plugins: [new HtmlWebpackPlugin({ template: 'src/index.html' })],
    module: {
      rules: [
        {
          include: /\.elm/,
          use: [
            'elm-hot-webpack-loader',
            { loader: 'elm-webpack-loader', options: { optimize: prod } },
          ],
        },
        { include: /\.css/, use: ['style-loader', 'css-loader'] },
      ],
    },
    devServer: {
      contentBase: 'public',
      watchContentBase: true,
      historyApiFallback: true,
      hot: true,
      overlay: true,
    },
  }
}
