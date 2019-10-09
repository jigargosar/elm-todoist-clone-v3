const Module = require('./Main.elm')
require('tachyons')
require('./style.css')

Module.Elm.Main.init({
  node: document.getElementById('root'),
})

// const W = Worker
const W = window['SharedWorker']
console.debug()
const w = new W('worker.js', { name: 'worker' })

console.log(w)

w.port.postMessage('hw')

w.port.onmessage = function(e) {
  console.log(e)
}
