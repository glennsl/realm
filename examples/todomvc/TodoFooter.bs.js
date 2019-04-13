// Generated by BUCKLESCRIPT VERSION 4.0.18, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Realm = require("../../src/Realm.bs.js");
var Actions = require("./Actions.bs.js");

function visibilityButton(uri, visibility, actualVisibility) {
  var match = visibility === actualVisibility;
  return Curry._4(Realm.React[/* Html */0][/* li */11], undefined, undefined, /* :: */[
              Curry._1(Realm.React[/* Html */0][/* Attr */0][/* onClick */4], Actions.changeVisibility(visibility)),
              /* [] */0
            ], /* :: */[
              Curry._5(Realm.React[/* Html */0][/* a */14], uri, undefined, undefined, match ? "selected" : "", /* :: */[
                    Curry._1(Realm.React[/* Html */0][/* text */2], visibility),
                    /* [] */0
                  ]),
              /* [] */0
            ]);
}

function clearButton(completedCount) {
  return Curry._4(Realm.React[/* Html */0][/* button */3], undefined, "clear-completed", /* :: */[
              Curry._1(Realm.React[/* Html */0][/* Attr */0][/* hidden */2], completedCount === 0),
              /* :: */[
                Curry._1(Realm.React[/* Html */0][/* Attr */0][/* onClick */4], Actions.clearCompleted),
                /* [] */0
              ]
            ], /* :: */[
              Curry._1(Realm.React[/* Html */0][/* text */2], "Clear completed"),
              /* [] */0
            ]);
}

function filters(visibility) {
  return Curry._4(Realm.React[/* Html */0][/* ul */10], undefined, "filters", undefined, /* :: */[
              visibilityButton("#/", "All", visibility),
              /* :: */[
                Curry._1(Realm.React[/* Html */0][/* text */2], " "),
                /* :: */[
                  visibilityButton("#/active", "Active", visibility),
                  /* :: */[
                    Curry._1(Realm.React[/* Html */0][/* text */2], " "),
                    /* :: */[
                      visibilityButton("#/completed", "Completed", visibility),
                      /* [] */0
                    ]
                  ]
                ]
              ]
            ]);
}

function view(entries, visibility) {
  var completedCount = Realm.Core[/* |> */11](Realm.Core[/* |> */11](entries, Curry._1(Realm.Core[/* List */3][/* filter */8], (function (entry) {
                  return entry[/* completed */2];
                }))), Realm.Core[/* List */3][/* length */10]);
  var activeCount = Curry._1(Realm.Core[/* List */3][/* length */10], entries) - completedCount | 0;
  return Curry._4(Realm.React[/* Html */0][/* footer */5], undefined, "footer", /* :: */[
              Curry._1(Realm.React[/* Html */0][/* Attr */0][/* hidden */2], entries === /* [] */0),
              /* [] */0
            ], /* :: */[
              Curry._4(Realm.React[/* Html */0][/* span */9], undefined, "todo-count", undefined, /* :: */[
                    Curry._4(Realm.React[/* Html */0][/* strong */12], undefined, undefined, undefined, /* :: */[
                          Curry._1(Realm.React[/* Html */0][/* text */2], Realm.Core[/* |> */11](activeCount, Realm.Core[/* String */7][/* fromInt */22])),
                          /* [] */0
                        ]),
                    /* :: */[
                      Curry._1(Realm.React[/* Html */0][/* text */2], " items"),
                      /* [] */0
                    ]
                  ]),
              /* :: */[
                filters(visibility),
                /* :: */[
                  clearButton(completedCount),
                  /* [] */0
                ]
              ]
            ]);
}

exports.visibilityButton = visibilityButton;
exports.clearButton = clearButton;
exports.filters = filters;
exports.view = view;
/* Realm Not a pure module */
