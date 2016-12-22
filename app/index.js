var Elm = require('../src/Main.elm');
var ports = require('../src/ports.js');

var app = Elm.Main.fullscreen();
ports.init(app);

// leak reference to app for injecting example
window.app = app;
