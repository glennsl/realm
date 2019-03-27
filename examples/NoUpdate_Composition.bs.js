// Generated by BUCKLESCRIPT VERSION 4.0.18, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Realm__Core = require("../src/core/Realm__Core.bs.js");
var Core__String = require("../src/core/Core__String.bs.js");
var RealmNoUpdate = require("../src/RealmNoUpdate.bs.js");

function init(param) {
  return /* record */[/* count */0];
}

var click = RealmNoUpdate.Cmd[/* make */0]((function (model) {
        return /* record */[/* count */model[/* count */0] + 1 | 0];
      }));

var Html = RealmNoUpdate.MakeHtml(/* module */[]);

function view(model) {
  var message = "You've clicked this " + (Core__String.fromInt(model[/* count */0]) + " times(s)");
  return Curry._4(Html[/* div */6], undefined, undefined, undefined, /* :: */[
              Curry._4(Html[/* button */4], undefined, undefined, /* :: */[
                    Curry._1(Html[/* Attr */0][/* onClick */4], click),
                    /* [] */0
                  ], /* :: */[
                    Curry._1(Html[/* text */3], message),
                    /* [] */0
                  ]),
              /* [] */0
            ]);
}

var Clicker = /* module */[
  /* init */init,
  /* click */click,
  /* Html */Html,
  /* view */view
];

function init$1(param) {
  return /* record */[
          /* show */true,
          /* n */0
        ];
}

var partial_arg = RealmNoUpdate.Task[/* randomInt */3];

var partial_arg$1 = RealmNoUpdate.Task[/* map */2];

var toggle = Realm__Core.$pipe$great(Realm__Core.$pipe$great((function (param) {
            return partial_arg(0, 10, param);
          }), (function (param, param$1) {
            return partial_arg$1((function (n, m) {
                          return /* record */[
                                  /* show */!m[/* show */0],
                                  /* n */n
                                ];
                        }), param, param$1);
          })), RealmNoUpdate.Cmd[/* fromTask */1]);

var Html$1 = RealmNoUpdate.MakeHtml(/* module */[]);

function view$1(greeting, model) {
  var match = model[/* show */0];
  return Curry._4(Html$1[/* div */6], undefined, undefined, undefined, /* :: */[
              Curry._4(Html$1[/* button */4], undefined, undefined, /* :: */[
                    Curry._1(Html$1[/* Attr */0][/* onClick */4], toggle),
                    /* [] */0
                  ], /* :: */[
                    Curry._1(Html$1[/* text */3], "Toggle greeting " + Core__String.fromInt(model[/* n */1])),
                    /* [] */0
                  ]),
              /* :: */[
                match ? Curry._1(Html$1[/* text */3], greeting) : Html$1[/* null */2],
                /* [] */0
              ]
            ]);
}

var Toggler = /* module */[
  /* init */init$1,
  /* toggle */toggle,
  /* Html */Html$1,
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

var Html$2 = RealmNoUpdate.MakeHtml(/* module */[]);

function clicker(model) {
  return Realm__Core.$pipe$great(view(model[/* clicker */0]), (function (param, param$1) {
                return RealmNoUpdate.map((function (model) {
                              return model[/* clicker */0];
                            }), (function (model, clickerModel) {
                              return /* record */[
                                      /* clicker */clickerModel,
                                      /* toggler */model[/* toggler */1]
                                    ];
                            }), param, param$1);
              }));
}

function toggler(greeting, model) {
  return Realm__Core.$pipe$great(view$1(greeting, model[/* toggler */1]), (function (param, param$1) {
                return RealmNoUpdate.map((function (model) {
                              return model[/* toggler */1];
                            }), (function (model, togglerModel) {
                              return /* record */[
                                      /* clicker */model[/* clicker */0],
                                      /* toggler */togglerModel
                                    ];
                            }), param, param$1);
              }));
}

var Components = /* module */[
  /* clicker */clicker,
  /* toggler */toggler
];

function view$2(greeting, model) {
  return Curry._4(Html$2[/* div */6], undefined, undefined, undefined, /* :: */[
              clicker(model),
              /* :: */[
                toggler(greeting, model),
                /* [] */0
              ]
            ]);
}

function mount(at) {
  var partial_arg = RealmNoUpdate.mountHtml(at);
  var func = function (param, param$1, param$2, param$3) {
    return partial_arg(init$2, param, param$1, param$2, param$3);
  };
  var arg = function (param) {
    return view$2("hello", param);
  };
  return (function (param, param$1) {
      return Curry._3(func, param, param$1, arg);
    });
}

exports.Clicker = Clicker;
exports.Toggler = Toggler;
exports.init = init$2;
exports.Html = Html$2;
exports.Components = Components;
exports.view = view$2;
exports.mount = mount;
/* click Not a pure module */
