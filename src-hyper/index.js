import { h, app } from 'hyperapp'

import validate from 'aproba'

const INC = state => state + 1
const DEC = state => state + 1


app({
  init: 0,
  view: state =>
    div({}, [
      h('h1', {}, state),
      h('button', { onClick: DEC }, '-'),
      h('button', { onClick: INC }, '+'),
    ]),
  node: document.getElementById('app'),
})


// HELPERS

const div = createEl('div')

function createEl(name) {
  validate('S', arguments)
  return function(props = {}, children = []) {
    validate('OA', arguments)
    return h(name, props, children)
  }
}
