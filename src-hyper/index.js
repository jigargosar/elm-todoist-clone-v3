import 'tachyons'
import './index.css'
import { app } from 'hyperapp'
import nanoid from 'nanoid'
import { div, i } from './html'

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
    div({ class: 'f2 pa2 flex' }, 'Projects'),
    state.pl.map(viewPrj),
  ])
}

function viewPrj(prj) {
  return div({ class: 'ph3 lh-copy flex h2' }, [
    div({ class: 'flex items-center' }, [
      i({ class: 'v-mid material-icons' }, 'folder'),
    ]),
    div({ class: 'flex items-center' }, prj.title),
  ])
}

// HELPERS
