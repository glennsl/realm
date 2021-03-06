// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var React = require("react");
var RealmOld = require("../../src/RealmOld.bs.js");

var Html = Curry._1(RealmOld.React[/* Html */0], /* module */[]);

function init(param) {
  return /* record */[/* count */0];
}

function update(msg, model) {
  return /* record */[/* count */model[/* count */0] + 1 | 0];
}

function view(model) {
  var message = "You've clicked this " + (String(model[/* count */0]) + " times(s)");
  return Curry._2(Html[/* div */3], /* [] */0, /* :: */[
              Curry._1(Html[/* fromReact */5], (function (dispatch) {
                      return React.createElement("button", {
                                  onClick: (function (_e) {
                                      return Curry._1(dispatch, /* Click */0);
                                    })
                                }, message);
                    })),
              /* [] */0
            ]);
}

function mount(at) {
  return RealmOld.React[/* mount */1](at)(init, update, view);
}

exports.Html = Html;
exports.init = init;
exports.update = update;
exports.view = view;
exports.mount = mount;
/* Html Not a pure module */
