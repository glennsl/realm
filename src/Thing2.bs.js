// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Realm = require("./Realm.bs.js");

function init() {
  return /* record */[
          /* count */0,
          /* show */true
        ];
}

function update(msg, model) {
  if (msg) {
    return /* record */[
            /* count */model[/* count */0],
            /* show */!model[/* show */1]
          ];
  } else {
    return /* record */[
            /* count */model[/* count */0] + 1 | 0,
            /* show */model[/* show */1]
          ];
  }
}

function view(greeting, model, dispatch) {
  var message = "You've clicked this " + (String(model[/* count */0]) + " times(s)");
  var match = model[/* show */1];
  return Realm.Html[/* div */2](/* None */0, /* :: */[
              Realm.Html[/* button */3](/* Some */[(function () {
                        return Curry._1(dispatch, /* Click */0);
                      })], /* :: */[
                    Realm.Html[/* text */1](message),
                    /* [] */0
                  ]),
              /* :: */[
                Realm.Html[/* button */3](/* Some */[(function () {
                          return Curry._1(dispatch, /* Toggle */1);
                        })], /* :: */[
                      Realm.Html[/* text */1]("Toggle greeting"),
                      /* [] */0
                    ]),
                /* :: */[
                  match ? Realm.Html[/* text */1](greeting) : Realm.Html[/* null */0],
                  /* [] */0
                ]
              ]
            ]);
}

exports.init = init;
exports.update = update;
exports.view = view;
/* Realm Not a pure module */