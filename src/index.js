import ports from './ports'

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
  maybeProjectId : null
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

const mockLabelList = [
  'to-read',
  'medical',
  'quick-ref',
].map((title, idx) => ({
  id: `LabelId-${nanoid()}`,
  title,
  createdAt: Date.now(),
  modifiedAt: Date.now(),
  hue: Math.round(Math.random() * 360),
  idx,
}))

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

const app = Module.Elm.Main.init({
  node: document.getElementById('root'),
  flags: { todoList: mockTodoList, projectList: mockProjectList, labelList : mockLabelList, filterList: mockFilterList },
})

const pubs = ports(
  [],
  { logError: err => console.error('Elm Error', err) },
  app,
)
console.debug(pubs)
