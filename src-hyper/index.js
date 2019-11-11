import { h, app } from 'hyperapp'

const INC = state => state + 1
const DEC = state => state + 1

app({
  init: 0,
  view: state =>
    h('main', {}, [
      h('h1', {}, state),
      h('button', { onClick: INC }, '-'),
      h('button', { onClick: DEC }, '+'),
    ]),
  node: document.getElementById('app'),
})
