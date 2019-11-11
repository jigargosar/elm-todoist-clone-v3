import validate from 'aproba'
import { h } from 'hyperapp'

export const div = createEl('div')
export const h1 = createEl('h1')
export const button = createEl('button')

function createEl(name) {
  validate('S', arguments)
  return function(props = {}, ...children) {
    return h(name, props, ...children)
  }
}
