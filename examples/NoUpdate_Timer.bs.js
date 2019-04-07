// Generated by BUCKLESCRIPT VERSION 4.0.18, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Realm = require("../src/Realm.bs.js");
var Core__String = require("../src/core/Core__String.bs.js");

var Html = Realm.MakeHtml(/* module */[]);

function init(param) {
  return Realm.Task[/* const */1](/* record */[
              /* count */0,
              /* mode : Fast */2
            ]);
}

function subs(model) {
  var tick = function (param) {
    return Realm.Effect[/* update */2]((function (model) {
                  return /* record */[
                          /* count */model[/* count */0] + 1 | 0,
                          /* mode */model[/* mode */1]
                        ];
                }));
  };
  var slow = Realm.Time[/* every */0]("slow", 2000, tick);
  var fast = Realm.Time[/* every */0]("fast", 1000, tick);
  var match = model[/* mode */1];
  switch (match) {
    case 0 : 
        return /* [] */0;
    case 1 : 
        return /* :: */[
                slow,
                /* [] */0
              ];
    case 2 : 
        return /* :: */[
                fast,
                /* [] */0
              ];
    case 3 : 
        return /* :: */[
                slow,
                /* :: */[
                  fast,
                  /* [] */0
                ]
              ];
    
  }
}

function setMode(mode) {
  return Realm.Effect[/* update */2]((function (model) {
                return /* record */[
                        /* count */model[/* count */0],
                        /* mode */mode
                      ];
              }));
}

function view(model) {
  var message = Core__String.fromInt(model[/* count */0]) + " seconds since page load";
  return Curry._4(Html[/* div */6], undefined, undefined, undefined, /* :: */[
              Curry._4(Html[/* div */6], undefined, undefined, undefined, /* :: */[
                    Curry._4(Html[/* button */4], undefined, undefined, /* :: */[
                          Curry._1(Html[/* Attr */0][/* onClick */4], setMode(/* Off */0)),
                          /* [] */0
                        ], /* :: */[
                          Curry._1(Html[/* text */3], "Off"),
                          /* [] */0
                        ]),
                    /* :: */[
                      Curry._4(Html[/* button */4], undefined, undefined, /* :: */[
                            Curry._1(Html[/* Attr */0][/* onClick */4], setMode(/* Slow */1)),
                            /* [] */0
                          ], /* :: */[
                            Curry._1(Html[/* text */3], "Slow"),
                            /* [] */0
                          ]),
                      /* :: */[
                        Curry._4(Html[/* button */4], undefined, undefined, /* :: */[
                              Curry._1(Html[/* Attr */0][/* onClick */4], setMode(/* Fast */2)),
                              /* [] */0
                            ], /* :: */[
                              Curry._1(Html[/* text */3], "Fast"),
                              /* [] */0
                            ]),
                        /* :: */[
                          Curry._4(Html[/* button */4], undefined, undefined, /* :: */[
                                Curry._1(Html[/* Attr */0][/* onClick */4], setMode(/* Both */3)),
                                /* [] */0
                              ], /* :: */[
                                Curry._1(Html[/* text */3], "Both"),
                                /* [] */0
                              ]),
                          /* [] */0
                        ]
                      ]
                    ]
                  ]),
              /* :: */[
                Curry._1(Html[/* text */3], message),
                /* [] */0
              ]
            ]);
}

function mount(at) {
  return Realm.mountHtml(at)(init, undefined, subs, view, /* () */0);
}

exports.Html = Html;
exports.init = init;
exports.subs = subs;
exports.setMode = setMode;
exports.view = view;
exports.mount = mount;
/* Html Not a pure module */
