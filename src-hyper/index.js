/*
import { h, app } from 'hyperapp'

app({
  init: 0,
  view: state =>
    h('main', {}, [
      h('h1', {}, state),
      h('button', { onClick: state => state - 1 }, '-'),
      h('button', { onClick: state => state + 1 }, '+'),
    ]),
  node: document.getElementById('app'),
})
*/

import { h, app } from 'hyperapp'
import picostyle from 'picostyle'
const React = h

const ps = picostyle(h)

const state = {
  text: 'Picostyle',
}

const actions = {}

const view = state => {
  // const keyColor = '#f07'
  const keyColor = '#000'

  const Text = ps('a')({
    fontSize: '64px',
    cursor: 'pointer',
    color: '#fff',
    padding: '0.4em',
    transition: 'all .2s ease-in-out',
    textDecoration: 'none',
    ':hover': {
      transform: 'scale(1.3)',
    },
    '@media (max-width: 450px)': {
      fontSize: '32px',
    },
  })

  const Wrapper = ps('div')({
    display: 'flex',
    justifyContent: 'center',
    alignItems: 'center',
    width: '100vw',
    height: '100vh',
    backgroundColor: keyColor,
  })
  return h('main', {}, [
    Wrapper(
      {},
      Text(
        { href: 'https://github.com/morishitter/picostyle' },
        state.text,
      ),
    ),
  ])
}

app({
  init: state,
  actions,
  view,
  node: document.getElementById('app'),
})
