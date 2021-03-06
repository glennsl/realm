// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Realm = require("../src/Realm.bs.js");

function init(param) {
  return Realm.Core[/* |> */11](Realm.Core[/* Time */17][/* now */0], Curry._1(Realm.Core[/* Future */14][/* map */3], (function (time) {
                    return /* record */[
                            /* time */time,
                            /* running */true
                          ];
                  })));
}

var update = Realm.Core[/* Fn */1][/* id */0];

function subs(model) {
  var tick = function (param) {
    return Curry._2(Realm.Core[/* Effect */15][/* do_ */3], (function (param) {
                  return Realm.Core[/* Time */17][/* now */0];
                }), (function (time, model) {
                  return /* record */[
                          /* time */time,
                          /* running */model[/* running */1]
                        ];
                }));
  };
  if (model[/* running */1]) {
    return /* :: */[
            Curry._3(Realm.Core[/* Time */17][/* every */2], "fast", 1000, tick),
            /* [] */0
          ];
  } else {
    return /* [] */0;
  }
}

var enable = Curry._1(Realm.Core[/* Effect */15][/* update */2], (function (model) {
        return /* record */[
                /* time */model[/* time */0],
                /* running */true
              ];
      }));

var disable = Curry._1(Realm.Core[/* Effect */15][/* update */2], (function (model) {
        return /* record */[
                /* time */model[/* time */0],
                /* running */false
              ];
      }));

function view(model) {
  return Curry._4(Realm.React[/* Html */0][/* div */4], undefined, undefined, undefined, /* :: */[
              Curry._1(Realm.React[/* Html */0][/* text */2], Curry._1(Realm.Core[/* Time */17][/* toString */3], model[/* time */0])),
              /* :: */[
                Curry._4(Realm.React[/* Html */0][/* div */4], undefined, undefined, undefined, /* :: */[
                      Curry._4(Realm.React[/* Html */0][/* button */3], undefined, undefined, /* :: */[
                            Curry._1(Realm.React[/* Html */0][/* Attr */0][/* onClick */4], disable),
                            /* [] */0
                          ], /* :: */[
                            Curry._1(Realm.React[/* Html */0][/* text */2], "Off"),
                            /* [] */0
                          ]),
                      /* :: */[
                        Curry._4(Realm.React[/* Html */0][/* button */3], undefined, undefined, /* :: */[
                              Curry._1(Realm.React[/* Html */0][/* Attr */0][/* onClick */4], enable),
                              /* [] */0
                            ], /* :: */[
                              Curry._1(Realm.React[/* Html */0][/* text */2], "On"),
                              /* [] */0
                            ]),
                        /* [] */0
                      ]
                    ]),
                /* [] */0
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
