// Generated by BUCKLESCRIPT VERSION 4.0.18, PLEASE EDIT WITH CARE
'use strict';

var Jest = require("@glennsl/bs-jest/src/jest.js");
var List = require("bs-platform/lib/js/list.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Realm = require("../src/Realm.bs.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");

Jest.describe("Task", (function (param) {
        Jest.testAsync("make", undefined, (function (finish) {
                var expected = "my-value";
                var task = Curry._1(Realm.Core[/* Task */14][/* make */0], (function (callback) {
                        return Curry._1(callback, expected);
                      }));
                return Curry._2(Realm.Core[/* Task */14][/* run */5], (function (value) {
                              return Curry._1(finish, Jest.Expect[/* toBe */2](expected, Jest.Expect[/* expect */0](value)));
                            }), task);
              }));
        Jest.testAsync("const", undefined, (function (finish) {
                var expected = "my-value";
                var task = Curry._1(Realm.Core[/* Task */14][/* const */1], expected);
                return Curry._2(Realm.Core[/* Task */14][/* run */5], (function (value) {
                              return Curry._1(finish, Jest.Expect[/* toBe */2](expected, Jest.Expect[/* expect */0](value)));
                            }), task);
              }));
        Jest.testAsync("andThen", undefined, (function (finish) {
                var task = Curry._2(Realm.Core[/* Task */14][/* andThen */2], (function (a) {
                        return Curry._1(Realm.Core[/* Task */14][/* const */1], a + "b");
                      }), Curry._1(Realm.Core[/* Task */14][/* const */1], "a"));
                return Curry._2(Realm.Core[/* Task */14][/* run */5], (function (value) {
                              return Curry._1(finish, Jest.Expect[/* toBe */2]("ab", Jest.Expect[/* expect */0](value)));
                            }), task);
              }));
        Jest.testAsync("map", undefined, (function (finish) {
                return Curry._2(Realm.Core[/* Task */14][/* run */5], (function (value) {
                              return Curry._1(finish, Jest.Expect[/* toBe */2](8, Jest.Expect[/* expect */0](value)));
                            }), Curry._2(Realm.Core[/* Task */14][/* map */3], (function (x) {
                                  return x + 5 | 0;
                                }), Curry._1(Realm.Core[/* Task */14][/* const */1], 3)));
              }));
        return Jest.testAsync("map2", undefined, (function (finish) {
                      return Curry._2(Realm.Core[/* Task */14][/* run */5], (function (value) {
                                    return Curry._1(finish, Jest.Expect[/* toBe */2](9, Jest.Expect[/* expect */0](value)));
                                  }), Curry._3(Realm.Core[/* Task */14][/* map2 */4], (function (x, y) {
                                        return x + y | 0;
                                      }), Curry._1(Realm.Core[/* Task */14][/* const */1], 3), Curry._1(Realm.Core[/* Task */14][/* const */1], 6)));
                    }));
      }));

function $$const(value) {
  return /* :: */[
          /* Update */Block.__(0, [(function (param) {
                  return value;
                })]),
          /* [] */0
        ];
}

function update(updater) {
  return /* :: */[
          /* Update */Block.__(0, [updater]),
          /* [] */0
        ];
}

function do_(action, mapper) {
  return /* :: */[
          /* Task */Block.__(1, [(function (model) {
                  return Curry._2(Realm.Core[/* Task */14][/* map */3], mapper, Curry._1(action, model));
                })]),
          /* [] */0
        ];
}

function andThen(last, param) {
  if (param) {
    var match = param[0];
    if (match.tag) {
      return /* :: */[
              /* Task */Block.__(1, [match[0]]),
              andThen(last, param[1])
            ];
    } else {
      return /* :: */[
              /* Update */Block.__(0, [match[0]]),
              andThen(last, param[1])
            ];
    }
  } else {
    return last;
  }
}

function map(get, set, param) {
  if (param) {
    var match = param[0];
    if (match.tag) {
      var f = match[0];
      return /* :: */[
              /* Task */Block.__(1, [(function (model) {
                      return Curry._2(Realm.Core[/* Task */14][/* map */3], (function (f, model) {
                                    return Curry._2(set, model, Curry._1(f, Curry._1(get, model)));
                                  }), Curry._1(f, Curry._1(get, model)));
                    })]),
              map(get, set, param[1])
            ];
    } else {
      var f$1 = match[0];
      return /* :: */[
              /* Update */Block.__(0, [(function (model) {
                      return Curry._2(set, model, Curry._1(f$1, Curry._1(get, model)));
                    })]),
              map(get, set, param[1])
            ];
    }
  } else {
    return /* [] */0;
  }
}

function step(model, param) {
  if (param) {
    var match = param[0];
    if (match.tag) {
      var rest = param[1];
      var next = Curry._2(Realm.Core[/* Task */14][/* map */3], (function (f$prime) {
              return /* :: */[
                      /* Update */Block.__(0, [f$prime]),
                      rest
                    ];
            }), Curry._1(match[0], model));
      return /* tuple */[
              undefined,
              Caml_option.some(next)
            ];
    } else {
      var rest$1 = param[1];
      var f = match[0];
      if (rest$1) {
        return /* tuple */[
                Caml_option.some(Curry._1(f, model)),
                Caml_option.some(Curry._1(Realm.Core[/* Task */14][/* const */1], rest$1))
              ];
      } else {
        return /* tuple */[
                Caml_option.some(Curry._1(f, model)),
                undefined
              ];
      }
    }
  } else {
    return /* tuple */[
            undefined,
            undefined
          ];
  }
}

var Effect = /* module */[
  /* none : [] */0,
  /* const */$$const,
  /* update */update,
  /* do_ */do_,
  /* andThen */andThen,
  /* map */map,
  /* step */step
];

Jest.describe("Effect", (function (param) {
        var run = function (effect, model, callback) {
          var aux = function (model, result, effect) {
            var match = step(model, effect);
            var next = match[1];
            var maybeValue = match[0];
            var match$1;
            if (maybeValue !== undefined) {
              var value = Caml_option.valFromOption(maybeValue);
              match$1 = /* tuple */[
                value,
                /* :: */[
                  value,
                  result
                ]
              ];
            } else {
              match$1 = /* tuple */[
                model,
                result
              ];
            }
            var result$1 = match$1[1];
            var model$1 = match$1[0];
            if (next !== undefined) {
              return Curry._2(Realm.Core[/* Task */14][/* run */5], (function (param) {
                            return aux(model$1, result$1, param);
                          }), Caml_option.valFromOption(next));
            } else {
              return Curry._1(callback, List.rev(result$1));
            }
          };
          return aux(model, /* [] */0, effect);
        };
        Jest.test("const", (function (param) {
                var effect_000 = /* Update */Block.__(0, [(function (param) {
                        return 42;
                      })]);
                var effect = /* :: */[
                  effect_000,
                  /* [] */0
                ];
                var match = step(0, effect);
                return Jest.Expect[/* toEqual */12](/* tuple */[
                            42,
                            undefined
                          ], Jest.Expect[/* expect */0](/* tuple */[
                                match[0],
                                match[1]
                              ]));
              }));
        Jest.test("update", (function (param) {
                var effect_000 = /* Update */Block.__(0, [(function (model) {
                        return model + 1 | 0;
                      })]);
                var effect = /* :: */[
                  effect_000,
                  /* [] */0
                ];
                var match = step(2, effect);
                return Jest.Expect[/* toEqual */12](/* tuple */[
                            3,
                            undefined
                          ], Jest.Expect[/* expect */0](/* tuple */[
                                match[0],
                                match[1]
                              ]));
              }));
        Jest.testAsync("do_", undefined, (function (finish) {
                var effect = do_((function (model) {
                        return Curry._1(Realm.Core[/* Task */14][/* const */1], model + 1 | 0);
                      }), (function (model, value) {
                        return model + value | 0;
                      }));
                return run(effect, 2, (function (result) {
                              return Curry._1(finish, Jest.Expect[/* toEqual */12](/* :: */[
                                              5,
                                              /* [] */0
                                            ], Jest.Expect[/* expect */0](result)));
                            }));
              }));
        Jest.testAsync("andThen", undefined, (function (finish) {
                var effect = andThen(/* :: */[
                      /* Update */Block.__(0, [(function (model) {
                              return /* record */[
                                      /* number */model[/* number */0],
                                      /* text */String(model[/* number */0])
                                    ];
                            })]),
                      /* [] */0
                    ], do_((function (model) {
                            return Curry._1(Realm.Core[/* Task */14][/* const */1], model[/* number */0] + 1 | 0);
                          }), (function (result, model) {
                            return /* record */[
                                    /* number */result,
                                    /* text */model[/* text */1]
                                  ];
                          })));
                return run(effect, /* record */[
                            /* number */1,
                            /* text */"foo"
                          ], (function (result) {
                              return Curry._1(finish, Jest.Expect[/* toEqual */12](/* :: */[
                                              /* record */[
                                                /* number */2,
                                                /* text */"foo"
                                              ],
                                              /* :: */[
                                                /* record */[
                                                  /* number */2,
                                                  /* text */"2"
                                                ],
                                                /* [] */0
                                              ]
                                            ], Jest.Expect[/* expect */0](result)));
                            }));
              }));
        return Jest.test("map", (function (param) {
                      var effect = map((function (model) {
                              return model[/* number */0];
                            }), (function (model, number) {
                              return /* record */[
                                      /* number */number,
                                      /* text */model[/* text */1]
                                    ];
                            }), /* :: */[
                            /* Update */Block.__(0, [(function (model) {
                                    return model + 1 | 0;
                                  })]),
                            /* [] */0
                          ]);
                      var match = step(/* record */[
                            /* number */4,
                            /* text */"bar"
                          ], effect);
                      return Jest.Expect[/* toEqual */12](/* tuple */[
                                  /* record */[
                                    /* number */5,
                                    /* text */"bar"
                                  ],
                                  undefined
                                ], Jest.Expect[/* expect */0](/* tuple */[
                                      match[0],
                                      match[1]
                                    ]));
                    }));
      }));

var Task = 0;

exports.Task = Task;
exports.Effect = Effect;
/*  Not a pure module */
