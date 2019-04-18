// Generated by BUCKLESCRIPT VERSION 5.0.2, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Realm = require("../src/Realm.bs.js");

function init(param) {
  return /* record */[/* count */0];
}

var click = Curry._1(Realm.Core[/* Effect */15][/* update */2], (function (model) {
        return /* record */[/* count */model[/* count */0] + 1 | 0];
      }));

function view(model) {
  var message = "You've clicked this " + (Curry._1(Realm.Core[/* String */7][/* fromInt */22], model[/* count */0]) + " times(s)");
  return Curry._4(Realm.React[/* Html */0][/* div */4], undefined, undefined, undefined, /* :: */[
              Curry._4(Realm.React[/* Html */0][/* button */3], undefined, undefined, /* :: */[
                    Curry._1(Realm.React[/* Html */0][/* Attr */0][/* onClick */4], click),
                    /* [] */0
                  ], /* :: */[
                    Curry._1(Realm.React[/* Html */0][/* text */2], message),
                    /* [] */0
                  ]),
              /* [] */0
            ]);
}

var Clicker = /* module */[
  /* init */init,
  /* click */click,
  /* view */view
];

function init$1(param) {
  return /* record */[
          /* show */true,
          /* n */0
        ];
}

var toggle = Curry._2(Realm.Core[/* Effect */15][/* do_ */3], (function (param) {
        return Curry._2(Realm.Core[/* Future */14][/* randomInt */6], 0, 10);
      }), (function (n, model) {
        return /* record */[
                /* show */!model[/* show */0],
                /* n */n
              ];
      }));

function view$1(greeting, model) {
  var match = model[/* show */0];
  return Curry._4(Realm.React[/* Html */0][/* div */4], undefined, undefined, undefined, /* :: */[
              Curry._4(Realm.React[/* Html */0][/* button */3], undefined, undefined, /* :: */[
                    Curry._1(Realm.React[/* Html */0][/* Attr */0][/* onClick */4], toggle),
                    /* [] */0
                  ], /* :: */[
                    Curry._1(Realm.React[/* Html */0][/* text */2], "Toggle greeting " + Curry._1(Realm.Core[/* String */7][/* fromInt */22], model[/* n */1])),
                    /* [] */0
                  ]),
              /* :: */[
                match ? Curry._1(Realm.React[/* Html */0][/* text */2], greeting) : Realm.React[/* Html */0][/* null */1],
                /* [] */0
              ]
            ]);
}

var Toggler = /* module */[
  /* init */init$1,
  /* toggle */toggle,
  /* view */view$1
];

function init$2(param) {
  return /* record */[
          /* clicker : record */[/* count */0],
          /* toggler : record */[
            /* show */true,
            /* n */0
          ]
        ];
}

function clicker(model) {
  return Realm.Core[/* |> */11](view(model[/* clicker */0]), Curry._1(Realm.React[/* Html */0][/* map */17], Curry._2(Realm.Core[/* Effect */15][/* map */5], (function (model) {
                        return model[/* clicker */0];
                      }), (function (model, clickerModel) {
                        return /* record */[
                                /* clicker */clickerModel,
                                /* toggler */model[/* toggler */1]
                              ];
                      }))));
}

function toggler(greeting, model) {
  return Realm.Core[/* |> */11](view$1(greeting, model[/* toggler */1]), Curry._1(Realm.React[/* Html */0][/* map */17], Curry._2(Realm.Core[/* Effect */15][/* map */5], (function (model) {
                        return model[/* toggler */1];
                      }), (function (model, togglerModel) {
                        return /* record */[
                                /* clicker */model[/* clicker */0],
                                /* toggler */togglerModel
                              ];
                      }))));
}

function view$2(model) {
  return Curry._4(Realm.React[/* Html */0][/* div */4], undefined, undefined, undefined, /* :: */[
              clicker(model),
              /* :: */[
                toggler("Hello", model),
                /* [] */0
              ]
            ]);
}

var App = Realm.React[/* SimpleApp */2](/* module */[
      /* init */init$2,
      /* view */view$2
    ]);

exports.Clicker = Clicker;
exports.Toggler = Toggler;
exports.App = App;
/* click Not a pure module */
