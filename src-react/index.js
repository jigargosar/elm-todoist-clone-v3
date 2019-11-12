import React from 'react'
import Button from '@material-ui/core/Button'
import ReactDOM from 'react-dom'
import { mapObjIndexed, prop, times, values } from 'ramda'
import CssBaseline from '@material-ui/core/CssBaseline'
import ListItem from '@material-ui/core/ListItem'
import List from '@material-ui/core/List'
import ListItemText from '@material-ui/core/ListItemText'
import { ListItemIcon } from '@material-ui/core'
import Icon from '@material-ui/core/Icon'
import {
  amber,
  blue,
  blueGrey,
  brown,
  cyan,
  deepOrange,
  deepPurple,
  green,
  grey,
  indigo,
  lightBlue,
  lightGreen,
  lime,
  orange,
  pink,
  purple,
  red,
  teal,
  yellow,
} from '@material-ui/core/colors'

const allHues = {
  red,
  amber,
  blue,
  blueGrey,
  brown,
  cyan,
  deepOrange,
  deepPurple,
  green,
  grey,
  indigo,
  lightBlue,
  lightGreen,
  lime,
  orange,
  pink,
  purple,
  teal,
  yellow,
}

const colors500 = mapObjIndexed(prop('500'), allHues)

const colors500List = values(colors500)

const nanoid = require('nanoid')

// require('tachyons')
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

// noinspection JSUnusedLocalSymbols
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

/*const app = Module.Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    now: Date.now(),
    todoList: mockTodoList,
    projectList: mockProjectList,
    labelList: mockLabelList,
    filterList: mockFilterList,
  },
})*/

function randomColor500() {
  const idx = Math.floor(Math.random() * colors500List.length)
  return colors500List[idx]
}

function App() {
  const projectList = mockProjectList
  return (
    <React.Fragment>
      <CssBaseline />
      <Button variant="contained" color="primary">
        Hello World
      </Button>
      <List>
        {projectList.map(p => (
          <ListItem key={p.id}>
            <ListItemIcon>
              <Icon style={{ color: randomColor500() }}>folder</Icon>
            </ListItemIcon>
            <ListItemText>{p.title}</ListItemText>
          </ListItem>
        ))}
      </List>
    </React.Fragment>
  )
}

ReactDOM.render(<App />, document.querySelector('#app'))
