import ports from './ports'

const Module = require('./Main.elm')
require('./style.css')
require('tachyons')

const app = Module.Elm.Main.init({
  node: document.getElementById('root'),
})

ports([], {}, app)
