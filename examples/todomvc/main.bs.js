// Generated by BUCKLESCRIPT VERSION 4.0.18, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Model = require("./Model.bs.js");
var Realm = require("../../src/Realm.bs.js");
var TodoApp = require("./TodoApp.bs.js");

function init(param) {
  return Curry._1(Realm.Core[/* Task */14][/* const */1], /* record */[
              /* entries : :: */[
                Model.Todo[/* make */1]("Item 1"),
                /* :: */[
                  Model.Todo[/* make */1]("Item 2"),
                  /* [] */0
                ]
              ],
              /* visibility */"All"
            ]);
}

var infoFooter = Curry._4(Realm.React[/* Html */0][/* footer */5], undefined, "info", undefined, /* :: */[
      Curry._4(Realm.React[/* Html */0][/* p */13], undefined, undefined, undefined, /* :: */[
            Curry._1(Realm.React[/* Html */0][/* text */2], "Double-click to edit a todo"),
            /* [] */0
          ]),
      /* :: */[
        Curry._4(Realm.React[/* Html */0][/* p */13], undefined, undefined, undefined, /* :: */[
              Curry._1(Realm.React[/* Html */0][/* text */2], "Written by "),
              /* :: */[
                Curry._5(Realm.React[/* Html */0][/* a */14], "https://github.com/glennsl", undefined, undefined, undefined, /* :: */[
                      Curry._1(Realm.React[/* Html */0][/* text */2], "Glenn Slotte"),
                      /* [] */0
                    ]),
                /* [] */0
              ]
            ]),
        /* :: */[
          Curry._4(Realm.React[/* Html */0][/* p */13], undefined, undefined, undefined, /* :: */[
                Curry._1(Realm.React[/* Html */0][/* text */2], "Part of "),
                /* :: */[
                  Curry._5(Realm.React[/* Html */0][/* a */14], "http://todomvc.com", undefined, undefined, undefined, /* :: */[
                        Curry._1(Realm.React[/* Html */0][/* text */2], "TodoMVC"),
                        /* [] */0
                      ]),
                  /* [] */0
                ]
              ]),
          /* [] */0
        ]
      ]
    ]);

function view(param) {
  return Curry._4(Realm.React[/* Html */0][/* div */4], undefined, undefined, undefined, /* :: */[
              TodoApp.view(param[/* entries */0], param[/* visibility */1]),
              /* :: */[
                infoFooter,
                /* [] */0
              ]
            ]);
}

var App = Realm.React[/* SimpleApp */2](/* module */[
      /* init */init,
      /* view */view
    ]);

exports.App = App;
/* App Not a pure module */
