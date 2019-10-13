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
  color:"hsla(3,70%,40%,1)",
  idx,
}))

const app = Module.Elm.Main.init({
  node: document.getElementById('root'),
  flags: { todoList: mockTodoList, projectList: mockProjectList },
})


const pubs = ports([], { logError: err => console.error('Elm Error', err) }, app)
console.debug(pubs)
