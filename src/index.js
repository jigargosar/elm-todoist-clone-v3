import ports from './ports'

const Module = require('./Main.elm')
require('tachyons')
require('./style.css')

const app = Module.Elm.Main.init({
  node: document.getElementById('root'),
})

ports([], {}, app)
