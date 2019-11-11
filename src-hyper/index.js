import 'tachyons'
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
    pl: ['Build gate', 'Exam Prep', 'Clone App'].map(createPrj),
  },
  view,
  node: document.getElementById('app'),
})

function view(state) {
  return div({}, [
    h1({}, state.ct),
    button({ onClick: DEC }, '-'),
    button({ onClick: INC }, '+'),
    div({ class: 'f2 pa2 flex' }, 'Projects'),
    state.pl.map(viewPrj),
  ])
}

function viewPrj(prj) {
  return div({ class: 'ph3 pv2' }, [prj.title])
}

// HELPERS

const div = createEl('div')
const h1 = createEl('h1')
const button = createEl('button')

function createEl(name) {
  validate('S', arguments)
  return function(props = {}, ...children) {
    return h(name, props, ...children)
  }
}
