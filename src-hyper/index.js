import { h, app } from 'hyperapp'

import validate from 'aproba'

const INC = state => state + 1
const DEC = state => state + 1


app({
  init: 0,
  view: state =>
    div({}, [
      h1({}, state),
      button({ onClick: DEC }, '-'),
      button({ onClick: INC }, '+'),
    ]),
  node: document.getElementById('app'),
})

// HELPERS

const div = createEl('div')
const h1 = createEl('h1')
const button = createEl('button')

function createEl(name) {
  validate('S', arguments)
  return function(props = {}, children = []) {
    validate('O*', arguments)
    return h(name, props, children)
  }
}
