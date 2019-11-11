import 'tachyons'
import './index.css'
import { app } from 'hyperapp'
import nanoid from 'nanoid'
import { a, div, i } from './html'
import { isNil, tap } from 'ramda'
import { onMouseMove, onMouseUp, preventDefault } from '@hyperapp/events'

const DRAG_START = (state, { event, prj }) => {
  event.preventDefault()
  const pos = { x: event.pageX, y: event.pageY }
  return {
    ...state,
    drag: { prj, start: pos, current: pos },
  }
}

const DRAG_MOVE = (state, event) => {
  const drag = state.drag
  if (isNil(drag)) return state
  const pos = { x: event.pageX, y: event.pageY }
  return {
    ...state,
    drag: { ...drag, current: pos },
  }
}

const DRAG_COMPLETE = state => {
  const drag = state.drag
  if (isNil(drag)) return state
  return {
    ...state,
    drag: null,
  }
}

function createPrj(title) {
  return { id: 'prj-' + nanoid(), title }
}

app({
  init: {
    ct: 0,
    pl: ['Build gate', 'Exam Prep', 'Clone App'].map(createPrj),
    drag: null,
  },
  view,
  node: document.getElementById('app'),
  subscriptions,
})

function subscriptions(state) {
  return [
    !isNil(state.drag) && [
      onMouseMove(DRAG_MOVE),
      onMouseUp(DRAG_COMPLETE),
    ],
  ]
}

function view(state) {
  return div({}, [
    div({ class: 'f2 lh-copy ph2 flex' }, 'Projects'),
    state.pl.map(viewPrj),
  ])
}

function viewPrj(prj) {
  return div({ class: 'ph3 flex' }, [
    div(
      {
        class: 'cur-move h2 w2 flex items-center justify-center',
        draggable: true,
        onDragstart: [
          DRAG_START,
          function(event) {
            return { event, prj }
          },
        ],
      },
      [i({ class: 'gray material-icons' }, 'folder')],
    ),
    a(
      {
        href: '#',
        class: 'link input-reset color-inherit ph1 flex items-center',
      },
      prj.title,
    ),
  ])
}

// HELPERS
