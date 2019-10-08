const Module = require('./Main.elm')
require("tachyons")
require("./style.css")

Module.Elm.Main.init({
  node: document.getElementById('root'),
})
