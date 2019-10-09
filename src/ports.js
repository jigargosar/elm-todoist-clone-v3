import { concat, isEmpty, mapObjIndexed, omit, pathOr } from 'ramda'

export default function ports(pubNames = [], subs = {}, app) {
  const pubs = pubNames.reduce((pubsAcc = {}, name) => {
    const send = pathOr(null, ['ports', name, 'send'])(app)
    if (!send) {
      console.warn('Elm Sub Port Not Found', name)
    }
    const pub = function(payload) {
      if (!send) {
        console.warn('Elm Sub Port Not Found. Send failed', {
          name,
          payload,
        })
      } else {
        send(payload)
      }
    }
    return Object.assign(pubsAcc, { [name]: pub })
  }, {})

  mapObjIndexed((fn, subName) => {
    const subscribe = pathOr(null, ['ports', subName, 'subscribe'])(app)
    if (!subscribe) {
      console.warn('Elm Cmd port Not Found', subName)
    } else {
      subscribe(payload => fn(payload, pubs))
    }
  })(subs)

  const unhandledPorts = omit(
    concat(Object.keys(subs), pubNames),
    app.ports,
  )

  const unhandledPortNames = Object.keys(unhandledPorts)
  if (!isEmpty(unhandledPortNames)) {
    console.warn('unhandledPorts', unhandledPortNames)
  }
  return pubs
}
