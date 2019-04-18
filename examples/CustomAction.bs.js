// Generated by BUCKLESCRIPT VERSION 5.0.2, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Realm = require("../src/Realm.bs.js");

function init(param) {
  return Curry._1(Realm.Core[/* Future */14][/* const */1], /* record */[/* count */0]);
}

function update(action) {
  if (action) {
    return Curry._1(Realm.Core[/* Effect */15][/* update */2], (function (model) {
                  return /* record */[/* count */model[/* count */0] - 1 | 0];
                }));
  } else {
    return Curry._1(Realm.Core[/* Effect */15][/* update */2], (function (model) {
                  return /* record */[/* count */model[/* count */0] + 1 | 0];
                }));
  }
}

function subs(param) {
  return /* [] */0;
}

function view(model) {
  return Curry._4(Realm.React[/* Html */0][/* div */4], undefined, undefined, undefined, /* :: */[
              Curry._4(Realm.React[/* Html */0][/* button */3], undefined, undefined, /* :: */[
                    Curry._1(Realm.React[/* Html */0][/* Attr */0][/* onClick */4], /* Increment */0),
                    /* [] */0
                  ], /* :: */[
                    Curry._1(Realm.React[/* Html */0][/* text */2], "+"),
                    /* [] */0
                  ]),
              /* :: */[
                Curry._1(Realm.React[/* Html */0][/* text */2], Realm.Core[/* |> */11](model[/* count */0], Realm.Core[/* String */7][/* fromInt */22])),
                /* :: */[
                  Curry._4(Realm.React[/* Html */0][/* button */3], undefined, undefined, /* :: */[
                        Curry._1(Realm.React[/* Html */0][/* Attr */0][/* onClick */4], /* Decrement */1),
                        /* [] */0
                      ], /* :: */[
                        Curry._1(Realm.React[/* Html */0][/* text */2], "-"),
                        /* [] */0
                      ]),
                  /* [] */0
                ]
              ]
            ]);
}

var App = Realm.React[/* App */1](/* module */[
      /* init */init,
      /* update */update,
      /* subs */subs,
      /* view */view
    ]);

exports.App = App;
/* App Not a pure module */
