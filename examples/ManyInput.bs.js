// Generated by BUCKLESCRIPT VERSION 5.0.2, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Realm = require("../src/Realm.bs.js");

function textInput(value) {
  return Curry._6(Realm.React[/* Html */0][/* input */16], undefined, /* `Text */[
              936573133,
              value
            ], undefined, undefined, /* :: */[
              Curry._1(Realm.React[/* Html */0][/* Attr */0][/* onInput */8], Realm.Core[/* Effect */15][/* const */1]),
              /* [] */0
            ], /* [] */0);
}

function init(param) {
  return /* record */[
          /* a */"",
          /* b */"",
          /* c */"",
          /* d */""
        ];
}

function view(model) {
  return Curry._4(Realm.React[/* Html */0][/* div */4], undefined, undefined, undefined, /* :: */[
              Realm.Core[/* |> */11](textInput(model[/* a */0]), Curry._1(Realm.React[/* Html */0][/* map */17], Curry._2(Realm.Core[/* Effect */15][/* map */5], (function (m) {
                              return m[/* a */0];
                            }), (function (m, v) {
                              return /* record */[
                                      /* a */v,
                                      /* b */m[/* b */1],
                                      /* c */m[/* c */2],
                                      /* d */m[/* d */3]
                                    ];
                            })))),
              /* :: */[
                Realm.Core[/* |> */11](textInput(model[/* b */1]), Curry._1(Realm.React[/* Html */0][/* map */17], Curry._2(Realm.Core[/* Effect */15][/* map */5], (function (m) {
                                return m[/* b */1];
                              }), (function (m, v) {
                                return /* record */[
                                        /* a */m[/* a */0],
                                        /* b */v,
                                        /* c */m[/* c */2],
                                        /* d */m[/* d */3]
                                      ];
                              })))),
                /* :: */[
                  Realm.Core[/* |> */11](textInput(model[/* c */2]), Curry._1(Realm.React[/* Html */0][/* map */17], Curry._2(Realm.Core[/* Effect */15][/* map */5], (function (m) {
                                  return m[/* c */2];
                                }), (function (m, v) {
                                  return /* record */[
                                          /* a */m[/* a */0],
                                          /* b */m[/* b */1],
                                          /* c */v,
                                          /* d */m[/* d */3]
                                        ];
                                })))),
                  /* :: */[
                    Realm.Core[/* |> */11](textInput(model[/* d */3]), Curry._1(Realm.React[/* Html */0][/* map */17], Curry._2(Realm.Core[/* Effect */15][/* map */5], (function (m) {
                                    return m[/* d */3];
                                  }), (function (m, v) {
                                    return /* record */[
                                            /* a */m[/* a */0],
                                            /* b */m[/* b */1],
                                            /* c */m[/* c */2],
                                            /* d */v
                                          ];
                                  })))),
                    /* [] */0
                  ]
                ]
              ]
            ]);
}

var App = Realm.React[/* SimpleApp */2](/* module */[
      /* init */init,
      /* view */view
    ]);

exports.textInput = textInput;
exports.App = App;
/* App Not a pure module */