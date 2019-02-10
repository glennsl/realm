// Generated by BUCKLESCRIPT VERSION 4.0.18, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var RealmNoUpdate = require("../src/RealmNoUpdate.bs.js");

var Html = RealmNoUpdate.MakeHtml(/* module */[]);

function init(param) {
  return /* record */[/* count */0];
}

var click = RealmNoUpdate.Cmd[/* make */0]((function (model) {
        return /* record */[/* count */model[/* count */0] + 1 | 0];
      }));

function view(model) {
  var message = "You've clicked this " + (String(model[/* count */0]) + " times(s)");
  return Curry._4(Html[/* div */7], undefined, undefined, undefined, /* :: */[
              Curry._4(Html[/* button */5], undefined, undefined, /* :: */[
                    Curry._1(Html[/* onClick */0], click),
                    /* [] */0
                  ], /* :: */[
                    Curry._1(Html[/* text */4], message),
                    /* [] */0
                  ]),
              /* [] */0
            ]);
}

function mount(at) {
  return RealmNoUpdate.mountHtml(at)(init, view);
}

exports.Html = Html;
exports.init = init;
exports.click = click;
exports.view = view;
exports.mount = mount;
/* Html Not a pure module */
