const Module = require('./Main.elm')
require('tachyons')
require('./style.css')

Module.Elm.Main.init({
  node: document.getElementById('root'),
})

const w = new SharedWorker('worker.js')

console.log(w)

w.port.postMessage('hw')

w.port.onmessage = function(e) {
  console.log(e)
}
