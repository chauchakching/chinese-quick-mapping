'use strict';

import './styles.css'
const {Elm} = require('./Main.elm')

// register service worker
if ('serviceWorker' in navigator) {
  window.addEventListener('load', () => {
    navigator.serviceWorker.register('/service-worker.js').then(registration => {
      console.log('SW registered: ', registration);
    }).catch(registrationError => {
      console.log('SW registration failed: ', registrationError);
    });
  });
}

function init() {
  // create root node
  const rootDiv = document.createElement('div')
  rootDiv.setAttribute('id', 'elm-node')
  document.body.appendChild(rootDiv)

  // retrieve localStorage data
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

  // init Elm
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
}

init()