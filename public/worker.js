globalThis.onconnect = function(e) {
  const port = e.ports[0]

  port.onmessage = function(e) {
    const workerResult = 'Result: ' + (e.data)
    console.log(workerResult)
    port.postMessage(workerResult);
  }

}