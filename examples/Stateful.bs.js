// Generated by BUCKLESCRIPT VERSION 4.0.7, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Realm = require("../src/Realm.bs.js");

var Html = Curry._1(Realm.React[/* Html */0], /* module */[]);

function init(param) {
  return /* record */[/* count */0];
}

function update(msg, model) {
  return /* record */[/* count */model[/* count */0] + 1 | 0];
}

function view(model) {
  var message = "You've clicked this " + (String(model[/* count */0]) + " times(s)");
  return Curry._2(Html[/* div */3], /* [] */0, /* :: */[
              Curry._2(Html[/* button */4], /* :: */[
                    Curry._1(Html[/* onClick */0], /* Click */0),
                    /* [] */0
                  ], /* :: */[
                    Curry._1(Html[/* text */2], message),
                    /* [] */0
                  ]),
              /* [] */0
            ]);
}

function mount(at) {
  return Realm.React[/* mount */1](at)(init, update, view);
}

exports.Html = Html;
exports.init = init;
exports.update = update;
exports.view = view;
exports.mount = mount;
/* Html Not a pure module */
