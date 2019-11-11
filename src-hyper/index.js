import 'tachyons'
import './index.css'
import { app } from 'hyperapp'
import nanoid from 'nanoid'
import { a, div, i } from './html'

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
    div({ class: 'f2 lh-copy ph2 flex' }, 'Projects'),
    state.pl.map(viewPrj),
  ])
}

function viewPrj(prj) {
  return div({ class: 'ph3 lh-copy flex' }, [
    div({ class: 'cur-move h2 w2 flex items-center justify-center' }, [
      i({ class: 'gray material-icons' }, 'folder'),
    ]),
    a({ href:"#", class: 'link input-reset color-inherit ph1 flex items-center' }, prj.title),
  ])
}

// HELPERS
