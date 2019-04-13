// Generated by BUCKLESCRIPT VERSION 4.0.18, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Realm = require("../../src/Realm.bs.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");

function create(description, id) {
  return /* record */[
          /* description */description,
          /* completed */false,
          /* editing */false,
          /* id */id
        ];
}

var Entry = /* module */[/* create */create];

var emptyModel = /* record */[
  /* entries : [] */0,
  /* field */"",
  /* uid */0,
  /* visibility */"All"
];

var LocalStorage = /* module */[];

var Json = /* module */[];

function init(param) {
  return Realm.Core[/* |> */11](Realm.Core[/* |> */11](Caml_option.nullable_to_opt(localStorage.getItem("realm-todo-save")), Curry._1(Realm.Core[/* Option */5][/* andThen */6], (function (prim) {
                        return Caml_option.nullable_to_opt(JSON.parse(prim));
                      }))), Curry._1(Realm.Core[/* Option */5][/* withDefault */0], emptyModel));
}

function persist(effect) {
  return Realm.Core[/* |> */11](effect, Curry._2(Realm.Core[/* Effect */15][/* map */5], (function (model) {
                    return model;
                  }), (function (param, model) {
                    Realm.Core[/* |> */11](Caml_option.nullable_to_opt(JSON.stringify(model)), Curry._1(Realm.Core[/* Option */5][/* map */1], (function (param) {
                                localStorage.setItem("realm-todo-save", param);
                                return /* () */0;
                              })));
                    return model;
                  })));
}

var add = Curry._1(Realm.Core[/* Effect */15][/* update */2], (function (model) {
        return /* record */[
                /* entries */model[/* field */1] === "" ? model[/* entries */0] : Pervasives.$at(model[/* entries */0], /* :: */[
                        create(model[/* field */1], model[/* uid */2]),
                        /* [] */0
                      ]),
                /* field */"",
                /* uid */model[/* uid */2] + 1 | 0,
                /* visibility */model[/* visibility */3]
              ];
      }));

function updateField(str) {
  return Curry._1(Realm.Core[/* Effect */15][/* update */2], (function (model) {
                return /* record */[
                        /* entries */model[/* entries */0],
                        /* field */str,
                        /* uid */model[/* uid */2],
                        /* visibility */model[/* visibility */3]
                      ];
              }));
}

function editingEntry(id, isEditing) {
  var updateEntry = function (t) {
    if (t[/* id */3] === id) {
      return /* record */[
              /* description */t[/* description */0],
              /* completed */t[/* completed */1],
              /* editing */isEditing,
              /* id */t[/* id */3]
            ];
    } else {
      return t;
    }
  };
  return Curry._1(Realm.Core[/* Effect */15][/* update */2], (function (model) {
                return /* record */[
                        /* entries */Curry._2(Realm.Core[/* List */3][/* map */4], updateEntry, model[/* entries */0]),
                        /* field */model[/* field */1],
                        /* uid */model[/* uid */2],
                        /* visibility */model[/* visibility */3]
                      ];
              }));
}

function updateEntry(id, task) {
  var updateEntry$1 = function (t) {
    if (t[/* id */3] === id) {
      return /* record */[
              /* description */task,
              /* completed */t[/* completed */1],
              /* editing */t[/* editing */2],
              /* id */t[/* id */3]
            ];
    } else {
      return t;
    }
  };
  return Curry._1(Realm.Core[/* Effect */15][/* update */2], (function (model) {
                return /* record */[
                        /* entries */Curry._2(Realm.Core[/* List */3][/* map */4], updateEntry$1, model[/* entries */0]),
                        /* field */model[/* field */1],
                        /* uid */model[/* uid */2],
                        /* visibility */model[/* visibility */3]
                      ];
              }));
}

function $$delete(id) {
  return Curry._1(Realm.Core[/* Effect */15][/* update */2], (function (model) {
                return /* record */[
                        /* entries */Curry._2(Realm.Core[/* List */3][/* filter */8], (function (t) {
                                return t[/* id */3] !== id;
                              }), model[/* entries */0]),
                        /* field */model[/* field */1],
                        /* uid */model[/* uid */2],
                        /* visibility */model[/* visibility */3]
                      ];
              }));
}

var deleteComplete = Curry._1(Realm.Core[/* Effect */15][/* update */2], (function (model) {
        return /* record */[
                /* entries */Curry._2(Realm.Core[/* List */3][/* filter */8], (function (t) {
                        return !t[/* completed */1];
                      }), model[/* entries */0]),
                /* field */model[/* field */1],
                /* uid */model[/* uid */2],
                /* visibility */model[/* visibility */3]
              ];
      }));

function check(id, isCompleted) {
  var updateEntry = function (t) {
    if (t[/* id */3] === id) {
      return /* record */[
              /* description */t[/* description */0],
              /* completed */isCompleted,
              /* editing */t[/* editing */2],
              /* id */t[/* id */3]
            ];
    } else {
      return t;
    }
  };
  return Curry._1(Realm.Core[/* Effect */15][/* update */2], (function (model) {
                return /* record */[
                        /* entries */Curry._2(Realm.Core[/* List */3][/* map */4], updateEntry, model[/* entries */0]),
                        /* field */model[/* field */1],
                        /* uid */model[/* uid */2],
                        /* visibility */model[/* visibility */3]
                      ];
              }));
}

function checkAll(isCompleted) {
  var updateEntry = function (t) {
    return /* record */[
            /* description */t[/* description */0],
            /* completed */isCompleted,
            /* editing */t[/* editing */2],
            /* id */t[/* id */3]
          ];
  };
  return Curry._1(Realm.Core[/* Effect */15][/* update */2], (function (model) {
                return /* record */[
                        /* entries */Curry._2(Realm.Core[/* List */3][/* map */4], updateEntry, model[/* entries */0]),
                        /* field */model[/* field */1],
                        /* uid */model[/* uid */2],
                        /* visibility */model[/* visibility */3]
                      ];
              }));
}

function changeVisibility(visibility) {
  return Curry._1(Realm.Core[/* Effect */15][/* update */2], (function (model) {
                return /* record */[
                        /* entries */model[/* entries */0],
                        /* field */model[/* field */1],
                        /* uid */model[/* uid */2],
                        /* visibility */visibility
                      ];
              }));
}

var update = persist;

function onEnter(action) {
  return Realm.Core[/* <| */12](Realm.React[/* Html */0][/* Attr */0][/* onKeyDown */9], (function (keyCode) {
                if (keyCode === 13) {
                  return action;
                } else {
                  return Realm.Core[/* Effect */15][/* none */0];
                }
              }));
}

function viewInput(task) {
  return Curry._4(Realm.React[/* Html */0][/* header */6], undefined, "header", undefined, /* :: */[
              Curry._4(Realm.React[/* Html */0][/* h1 */7], undefined, undefined, undefined, /* :: */[
                    Curry._1(Realm.React[/* Html */0][/* text */2], "todos"),
                    /* [] */0
                  ]),
              /* :: */[
                Curry._6(Realm.React[/* Html */0][/* input */16], "What needs to be done?", /* `Text */[
                      936573133,
                      task
                    ], undefined, "new-todo", /* :: */[
                      Curry._1(Realm.React[/* Html */0][/* Attr */0][/* autofocus */1], true),
                      /* :: */[
                        Curry._1(Realm.React[/* Html */0][/* Attr */0][/* name */3], "newTodo"),
                        /* :: */[
                          Curry._1(Realm.React[/* Html */0][/* Attr */0][/* onInput */8], updateField),
                          /* :: */[
                            onEnter(add),
                            /* [] */0
                          ]
                        ]
                      ]
                    ], /* [] */0),
                /* [] */0
              ]
            ]);
}

function viewEntry(todo) {
  var partial_arg = todo[/* id */3];
  return Curry._4(Realm.React[/* Html */0][/* li */11], undefined, todo[/* editing */2] ? "editing" : "", undefined, /* :: */[
              Curry._4(Realm.React[/* Html */0][/* div */4], undefined, "view", undefined, /* :: */[
                    Curry._6(Realm.React[/* Html */0][/* input */16], undefined, /* `Checkbox */[
                          111644259,
                          todo[/* completed */1]
                        ], undefined, "toggle", /* :: */[
                          Curry._1(Realm.React[/* Html */0][/* Attr */0][/* onClick */4], check(todo[/* id */3], !todo[/* completed */1])),
                          /* [] */0
                        ], /* [] */0),
                    /* :: */[
                      Curry._5(Realm.React[/* Html */0][/* label */15], undefined, /* :: */[
                            Curry._1(Realm.React[/* Html */0][/* Attr */0][/* onDoubleClick */5], editingEntry(todo[/* id */3], true)),
                            /* [] */0
                          ], undefined, undefined, /* :: */[
                            Curry._1(Realm.React[/* Html */0][/* text */2], todo[/* description */0]),
                            /* [] */0
                          ]),
                      /* :: */[
                        Curry._4(Realm.React[/* Html */0][/* button */3], undefined, "destroy", /* :: */[
                              Curry._1(Realm.React[/* Html */0][/* Attr */0][/* onClick */4], $$delete(todo[/* id */3])),
                              /* [] */0
                            ], /* [] */0),
                        /* [] */0
                      ]
                    ]
                  ]),
              /* :: */[
                Curry._6(Realm.React[/* Html */0][/* input */16], undefined, /* `Text */[
                      936573133,
                      todo[/* description */0]
                    ], "todo-" + String(todo[/* id */3]), "edit", /* :: */[
                      Curry._1(Realm.React[/* Html */0][/* Attr */0][/* onInput */8], (function (param) {
                              return updateEntry(partial_arg, param);
                            })),
                      /* :: */[
                        Curry._1(Realm.React[/* Html */0][/* Attr */0][/* onBlur */7], editingEntry(todo[/* id */3], false)),
                        /* :: */[
                          onEnter(editingEntry(todo[/* id */3], false)),
                          /* [] */0
                        ]
                      ]
                    ], /* [] */0),
                /* [] */0
              ]
            ]);
}

function viewEntries(visibility, entries) {
  var isVisible = function (todo) {
    switch (visibility) {
      case "Active" : 
          return !todo[/* completed */1];
      case "Completed" : 
          return todo[/* completed */1];
      default:
        return true;
    }
  };
  var allCompleted = Curry._2(Realm.Core[/* List */3][/* all */13], (function (t) {
          return t[/* completed */1];
        }), entries);
  return Curry._4(Realm.React[/* Html */0][/* section */8], undefined, "main", undefined, /* :: */[
              Curry._6(Realm.React[/* Html */0][/* input */16], undefined, /* `Checkbox */[
                    111644259,
                    allCompleted
                  ], undefined, "toggle-all", /* :: */[
                    Curry._1(Realm.React[/* Html */0][/* Attr */0][/* name */3], "toggle-all"),
                    /* :: */[
                      Curry._1(Realm.React[/* Html */0][/* Attr */0][/* onClick */4], checkAll(!allCompleted)),
                      /* [] */0
                    ]
                  ], /* [] */0),
              /* :: */[
                Curry._5(Realm.React[/* Html */0][/* label */15], "toggle-all", undefined, undefined, undefined, /* :: */[
                      Curry._1(Realm.React[/* Html */0][/* text */2], "Mark all as complete"),
                      /* [] */0
                    ]),
                /* :: */[
                  Realm.Core[/* <| */12]((function (eta) {
                          var func = Realm.React[/* Html */0][/* ul */10];
                          return Curry._2((function (param) {
                                          return Curry._2(func, param, "todo-list");
                                        })(undefined), undefined, eta);
                        }), Realm.Core[/* |> */11](Realm.Core[/* |> */11](entries, Curry._1(Realm.Core[/* List */3][/* filter */8], isVisible)), Curry._1(Realm.Core[/* List */3][/* map */4], viewEntry))),
                  /* [] */0
                ]
              ]
            ]);
}

function viewControlsCount(entriesLeft) {
  var item = entriesLeft === 1 ? " item" : " items";
  return Curry._4(Realm.React[/* Html */0][/* span */9], undefined, "todo-count", undefined, /* :: */[
              Curry._4(Realm.React[/* Html */0][/* strong */12], undefined, undefined, undefined, /* :: */[
                    Curry._1(Realm.React[/* Html */0][/* text */2], Curry._1(Realm.Core[/* String */7][/* fromInt */22], entriesLeft)),
                    /* [] */0
                  ]),
              /* :: */[
                Curry._1(Realm.React[/* Html */0][/* text */2], item + " left"),
                /* [] */0
              ]
            ]);
}

function visibilitySwap(uri, visibility, actualVisibility) {
  return Curry._4(Realm.React[/* Html */0][/* li */11], undefined, undefined, /* :: */[
              Curry._1(Realm.React[/* Html */0][/* Attr */0][/* onClick */4], changeVisibility(visibility)),
              /* [] */0
            ], /* :: */[
              Curry._5(Realm.React[/* Html */0][/* a */14], uri, undefined, undefined, visibility === actualVisibility ? "selected" : "", /* :: */[
                    Curry._1(Realm.React[/* Html */0][/* text */2], visibility),
                    /* [] */0
                  ]),
              /* [] */0
            ]);
}

function viewControlsFilters(visibility) {
  return Curry._4(Realm.React[/* Html */0][/* ul */10], undefined, "filters", undefined, /* :: */[
              visibilitySwap("#/", "All", visibility),
              /* :: */[
                Curry._1(Realm.React[/* Html */0][/* text */2], " "),
                /* :: */[
                  visibilitySwap("#/active", "Active", visibility),
                  /* :: */[
                    Curry._1(Realm.React[/* Html */0][/* text */2], " "),
                    /* :: */[
                      visibilitySwap("#/completed", "Completed", visibility),
                      /* [] */0
                    ]
                  ]
                ]
              ]
            ]);
}

function viewControlsClear(entriesCompleted) {
  return Curry._4(Realm.React[/* Html */0][/* button */3], undefined, "clear-completed", /* :: */[
              Curry._1(Realm.React[/* Html */0][/* Attr */0][/* hidden */2], entriesCompleted === 0),
              /* :: */[
                Curry._1(Realm.React[/* Html */0][/* Attr */0][/* onClick */4], deleteComplete),
                /* [] */0
              ]
            ], /* :: */[
              Curry._1(Realm.React[/* Html */0][/* text */2], "Clear completed (" + (Curry._1(Realm.Core[/* String */7][/* fromInt */22], entriesCompleted) + ")")),
              /* [] */0
            ]);
}

function viewControls(visibility, entries) {
  var entriesCompleted = Realm.Core[/* |> */11](Realm.Core[/* |> */11](entries, Curry._1(Realm.Core[/* List */3][/* filter */8], (function (t) {
                  return t[/* completed */1];
                }))), Realm.Core[/* List */3][/* length */10]);
  var entriesLeft = Curry._1(Realm.Core[/* List */3][/* length */10], entries) - entriesCompleted | 0;
  return Curry._4(Realm.React[/* Html */0][/* footer */5], undefined, "footer", /* :: */[
              Curry._1(Realm.React[/* Html */0][/* Attr */0][/* hidden */2], entries === /* [] */0),
              /* [] */0
            ], /* :: */[
              viewControlsCount(entriesLeft),
              /* :: */[
                viewControlsFilters(visibility),
                /* :: */[
                  viewControlsClear(entriesCompleted),
                  /* [] */0
                ]
              ]
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
                Curry._1(Realm.React[/* Html */0][/* text */2], "Based on "),
                /* :: */[
                  Curry._5(Realm.React[/* Html */0][/* a */14], "https://github.com/evancz/elm-todomvc", undefined, undefined, undefined, /* :: */[
                        Curry._1(Realm.React[/* Html */0][/* text */2], "evancz/elm-todomvc"),
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
      ]
    ]);

function view(model) {
  return Curry._4(Realm.React[/* Html */0][/* div */4], undefined, "todomvc-wrapper", undefined, /* :: */[
              Curry._4(Realm.React[/* Html */0][/* section */8], undefined, "todoapp", undefined, /* :: */[
                    viewInput(model[/* field */1]),
                    /* :: */[
                      viewEntries(model[/* visibility */3], model[/* entries */0]),
                      /* :: */[
                        viewControls(model[/* visibility */3], model[/* entries */0]),
                        /* [] */0
                      ]
                    ]
                  ]),
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

exports.Entry = Entry;
exports.emptyModel = emptyModel;
exports.LocalStorage = LocalStorage;
exports.Json = Json;
exports.init = init;
exports.persist = persist;
exports.add = add;
exports.updateField = updateField;
exports.editingEntry = editingEntry;
exports.updateEntry = updateEntry;
exports.$$delete = $$delete;
exports.deleteComplete = deleteComplete;
exports.check = check;
exports.checkAll = checkAll;
exports.changeVisibility = changeVisibility;
exports.update = update;
exports.onEnter = onEnter;
exports.viewInput = viewInput;
exports.viewEntry = viewEntry;
exports.viewEntries = viewEntries;
exports.viewControlsCount = viewControlsCount;
exports.visibilitySwap = visibilitySwap;
exports.viewControlsFilters = viewControlsFilters;
exports.viewControlsClear = viewControlsClear;
exports.viewControls = viewControls;
exports.infoFooter = infoFooter;
exports.view = view;
exports.App = App;
/* add Not a pure module */
