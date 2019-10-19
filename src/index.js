import ports from './ports'
import { times } from 'ramda'

const nanoid = require('nanoid')
const Module = require('./Main.elm')
require('tachyons')
require('./style.css')

const mockTodoList = [
  'Get Milk',
  'Remember to call',
  'Do Stuff!',
  'And More',
].map((title, idx) => ({
  id: `TodoId-${nanoid()}`,
  title,
  createdAt: Date.now(),
  modifiedAt: Date.now(),
  isCompleted: Math.random() > 0.3,
  idx,
  projectRef: null,
  labelIdList: []
}))

const mockProjectList = [
  'Clone This',
  'Finish That',
  'Read GGG',
  'Complete All Foo',
].map((title, idx) => ({
  id: `ProjectId-${nanoid()}`,
  title,
  createdAt: Date.now(),
  modifiedAt: Date.now(),
  hue: Math.round(Math.random() * 360),
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

times(moveRandomTodoToRandomProject, 3)
times(assignRandomLabelsToRandomTodo, 7)

const app = Module.Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    todoList: mockTodoList,
    projectList: mockProjectList,
    labelList: mockLabelList,
    filterList: mockFilterList,
  },
})

const pubs = ports(
  [],
  { logError: err => console.error('Elm Error', err) },
  app,
)
console.debug(pubs)
