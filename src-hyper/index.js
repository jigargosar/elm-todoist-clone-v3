import { h, app } from 'hyperapp'

import validate from 'aproba'
import nanoid from 'nanoid'

const INC = state => {
  return state.ct + 1
}
const DEC = state => {
  return state.ct + 1
}

function createPrj(title) {
  return { id: 'prj-' + nanoid(), title }
}

app({
  init: {
    ct: 0,
    pl: ['p1', 'p2', 'p3'].map(createPrj),
  },
  view,
  node: document.getElementById('app'),
})

function view(state) {
  return div({}, [
    h1({}, state.ct),
    button({ onClick: DEC }, '-'),
    button({ onClick: INC }, '+'),
  ])
}

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
