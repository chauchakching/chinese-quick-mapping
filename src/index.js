'use strict';

import './index.html'
const {Elm} = require('./Main.elm')

let flags = {
  inputHistory: [],
  quickMapping: {}
}
try {
  flags = {
    inputHistory: JSON.parse(localStorage.getItem('storage')).inputHistory,
    quickMapping: JSON.parse(localStorage.getItem('quickMapping'))
  }
} catch (e) {}

const app = Elm.Main.init({ node: document.getElementById('elm-node'), flags: flags })

app.ports.select.subscribe(function(id) {
  document.getElementById(id).select()
})
app.ports.setStorage.subscribe(function(x) {
  localStorage.setItem('storage', JSON.stringify(x))
})
app.ports.setQuickMapping.subscribe(function(x) {
  localStorage.setItem('quickMapping', JSON.stringify(x))
})
