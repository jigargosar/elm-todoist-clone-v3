import ports from './ports'
import { append, equals, isEmpty, isNil, reject, times, uniq } from 'ramda'

const nanoid = require('nanoid')
const Module = require('./Main.elm')
require('tachyons')
require('bulma')
require('./style.css')

function randomCColor() {
  return Math.round(Math.random() * 6)
}

const mockTodoList = [
  'Get Milk',
  'Remember to call',
  'Do Stuff!',
  'And More',
  'Read Chapter 1',
  'fry toast',
  'cook food',
  'take dog for a walk',
  'workout',
].map((title, idx) => ({
  id: `TodoId-${nanoid()}`,
  title,
  createdAt: Date.now(),
  modifiedAt: Date.now(),
  isCompleted: Math.random() > 0.3,
  idx,
  projectRef: null,
  labelIdList: [],
}))

const mockProjectList = [
  'Clone This',
  'Finish That',
  'Read GOT',
  'Complete All Foo',
].map((title, idx) => ({
  id: `ProjectId-${nanoid()}`,
  title,
  createdAt: Date.now(),
  modifiedAt: Date.now(),
  hue: Math.round(Math.random() * 360),
  cColor: randomCColor(),
  idx,
}))

function getRandomListItem(list) {
  const randomIdx = Math.floor(Math.random() * list.length)
  return list[randomIdx]
}

const mockLabelList = ['to-read', 'medical', 'quick-ref'].map(
  (title, idx) => ({
    id: `LabelId-${nanoid()}`,
    title,
    createdAt: Date.now(),
    modifiedAt: Date.now(),
    hue: Math.round(Math.random() * 360),
    cColor: randomCColor(),
    idx,
  }),
)

const mockFilterList = [
  'Assigned to me',
  'Assigned to others',
  'Priority 1',
  'Priority 2',
  'Priority 3',
  'View all',
  'No Due Date',
].map((title, idx) => ({
  id: `FilterId-${nanoid()}`,
  title,
  createdAt: Date.now(),
  modifiedAt: Date.now(),
  hue: Math.round(Math.random() * 360),
  cColor: randomCColor(),
  idx,
}))

function moveRandomTodoToRandomProject() {
  const randomTodo = getRandomListItem(mockTodoList)
  const randomProject = getRandomListItem(mockProjectList)
  randomTodo.projectRef = randomProject.id
}

function assignRandomLabelsToRandomTodo() {
  const randomTodo = getRandomListItem(mockTodoList)
  const randomLabel = getRandomListItem(mockLabelList)
  randomTodo.labelIdList.push(randomLabel.id)
}

times(moveRandomTodoToRandomProject, 10)
times(assignRandomLabelsToRandomTodo, 7)

const app = Module.Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    now: Date.now(),
    todoList: mockTodoList,
    projectList: mockProjectList,
    labelList: mockLabelList,
    filterList: mockFilterList,
  },
})

let nullableActiveElementOnDocumentBlur = null
document.addEventListener(
  'focus',
  function(event) {
    console.log('onFocus activeElement', document.activeElement, event)
    if (!isNil(nullableActiveElementOnDocumentBlur)) {
      nullableActiveElementOnDocumentBlur.focus()
      nullableActiveElementOnDocumentBlur = null
    }
  },
  { capture: true, passive: true },
)

document.addEventListener(
  'blur',
  function(event) {
    console.log('onBlur activeElement', document.activeElement, event)
    nullableActiveElementOnDocumentBlur = document.activeElement
  },
  { capture: true, passive: true },
)

customElements.define(
  'autofocus-on-connect',
  class extends HTMLElement {
    constructor() {
      super()
      console.log('autofocus created')
    }
  },
)

const monitorFocusOrClickOutside = MonitorFocusOrClickOutside()
const pubs = ports(
  ['onFocusOrClickOutside'],
  {
    logError: err => console.error('Elm Error', err),
    registerOnFocusOrClickOutSide: domId =>
      monitorFocusOrClickOutside.add(domId),
    unRegisterOnFocusOrClickOutSide: domId =>
      monitorFocusOrClickOutside.remove(domId),
  },
  app,
)
console.debug(pubs)

function MonitorFocusOrClickOutside() {
  let domIdList = []
  const listener = e => {
    if (isEmpty(domIdList)) return
    const target = e.target
    // console.log(target, e.path, domIdList)
    domIdList.forEach(domId => {
      const monitorEl = document.getElementById(domId)
      if (
        monitorEl &&
        monitorEl !== target &&
        !monitorEl.contains(target)
      ) {
        pubs.onFocusOrClickOutside(domId)
      }
    })
  }
  document.addEventListener('focusin', listener)
  document.addEventListener('click', listener)
  return {
    add(domId) {
      domIdList = uniq(append(domId, domIdList))
    },
    remove(domId) {
      domIdList = reject(equals(domId), domIdList)
    },
  }
}
