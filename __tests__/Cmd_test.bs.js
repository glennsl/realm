// Generated by BUCKLESCRIPT VERSION 4.0.18, PLEASE EDIT WITH CARE
'use strict';

var Jest = require("@glennsl/bs-jest/src/jest.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");

function make(f) {
  return f;
}

function $$const(value) {
  return (function (f) {
      return Curry._1(f, value);
    });
}

function map(f, task, resolve) {
  return Curry._1(task, (function (value) {
                return Curry._1(resolve, Curry._1(f, value));
              }));
}

function run(receiver, task) {
  return Curry._1(task, receiver);
}

function andThen(f, task, resolve) {
  return Curry._1(task, (function (value) {
                return Curry._2(f, value, resolve);
              }));
}

var Task = /* module */[
  /* make */make,
  /* const */$$const,
  /* map */map,
  /* andThen */andThen,
  /* run */run
];

Jest.describe("Task", (function (param) {
        Jest.testAsync("make", undefined, (function (finish) {
                var expected = "my-value";
                return Curry._1(finish, Jest.Expect[/* toBe */2](expected, Jest.Expect[/* expect */0](expected)));
              }));
        Jest.testAsync("map", undefined, (function (finish) {
                var f = function (x) {
                  return x + 5 | 0;
                };
                var value = Curry._1(f, 3);
                return Curry._1(finish, Jest.Expect[/* toBe */2](8, Jest.Expect[/* expect */0](value)));
              }));
        return Jest.testAsync("const", undefined, (function (finish) {
                      var expected = "my-value";
                      return Curry._1(finish, Jest.Expect[/* toBe */2](expected, Jest.Expect[/* expect */0](expected)));
                    }));
      }));

function $$const$1(value) {
  return /* Update */Block.__(0, [
            (function (param) {
                return value;
              }),
            /* End */0
          ]);
}

function update(updater) {
  return /* Update */Block.__(0, [
            updater,
            /* End */0
          ]);
}

function do_(action, mapper) {
  return /* Task */Block.__(1, [
            (function (model) {
                var partial_arg = Curry._1(action, model);
                return (function (param) {
                    return Curry._1(partial_arg, (function (value) {
                                  return Curry._1(param, Curry._1(mapper, value));
                                }));
                  });
              }),
            /* End */0
          ]);
}

function andThen$1(last, param) {
  if (typeof param === "number") {
    return last;
  } else if (param.tag) {
    return /* Task */Block.__(1, [
              param[0],
              andThen$1(last, param[1])
            ]);
  } else {
    return /* Update */Block.__(0, [
              param[0],
              andThen$1(last, param[1])
            ]);
  }
}

function step(model, param) {
  if (typeof param === "number") {
    return /* tuple */[
            undefined,
            undefined
          ];
  } else if (param.tag) {
    var next = param[1];
    var partial_arg = Curry._1(param[0], model);
    return /* tuple */[
            undefined,
            Caml_option.some((function (param) {
                    var f = function (updater) {
                      return /* Update */Block.__(0, [
                                updater,
                                next
                              ]);
                    };
                    return Curry._1(partial_arg, (function (value) {
                                  return Curry._1(param, Curry._1(f, value));
                                }));
                  }))
          ];
  } else {
    var next$1 = param[1];
    var match = next$1 === /* End */0;
    return /* tuple */[
            Caml_option.some(Curry._1(param[0], model)),
            match ? undefined : Caml_option.some((function (f) {
                      return Curry._1(f, next$1);
                    }))
          ];
  }
}

var Effect = /* module */[
  /* const */$$const$1,
  /* update */update,
  /* do_ */do_,
  /* andThen */andThen$1,
  /* step */step
];

Jest.describe("Effect", (function (param) {
        Jest.test("const", (function (param) {
                var effect_000 = function (param) {
                  return 42;
                };
                var effect = /* Update */Block.__(0, [
                    effect_000,
                    /* End */0
                  ]);
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
                var effect_000 = function (model) {
                  return model + 1 | 0;
                };
                var effect = /* Update */Block.__(0, [
                    effect_000,
                    /* End */0
                  ]);
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
                        var value = model + 1 | 0;
                        return (function (f) {
                            return Curry._1(f, value);
                          });
                      }), (function (model, value) {
                        return model + value | 0;
                      }));
                var match = step(2, effect);
                var next = match[1];
                var value1 = match[0];
                if (next !== undefined) {
                  return Curry._1(Caml_option.valFromOption(next), (function (effect) {
                                var model = value1 !== undefined ? value1 : 2;
                                var match = step(model, effect);
                                return Curry._1(finish, Jest.Expect[/* toEqual */12](/* tuple */[
                                                undefined,
                                                5,
                                                undefined
                                              ], Jest.Expect[/* expect */0](/* tuple */[
                                                    value1,
                                                    match[0],
                                                    match[1]
                                                  ])));
                              }));
                } else {
                  return Curry._1(finish, Jest.fail("should be more steps"));
                }
              }));
        return Jest.testAsync("andThen", undefined, (function (finish) {
                      var effect = andThen$1(/* Update */Block.__(0, [
                              (function (model) {
                                  return model / 2 | 0;
                                }),
                              /* End */0
                            ]), do_((function (model) {
                                  var value = String(model) + "1";
                                  return (function (f) {
                                      return Curry._1(f, value);
                                    });
                                }), (function (result, model) {
                                  return model + Caml_format.caml_int_of_string(result) | 0;
                                })));
                      var match = step(3, effect);
                      var next = match[1];
                      var value1 = match[0];
                      if (next !== undefined) {
                        return Curry._1(Caml_option.valFromOption(next), (function (effect) {
                                      var model = value1 !== undefined ? value1 : 3;
                                      var match = step(model, effect);
                                      var next = match[1];
                                      var value2 = match[0];
                                      if (next !== undefined) {
                                        return Curry._1(Caml_option.valFromOption(next), (function (effect) {
                                                      var model$1 = value2 !== undefined ? value2 : model;
                                                      var match = step(model$1, effect);
                                                      return Curry._1(finish, Jest.Expect[/* toEqual */12](/* tuple */[
                                                                      undefined,
                                                                      34,
                                                                      17,
                                                                      undefined
                                                                    ], Jest.Expect[/* expect */0](/* tuple */[
                                                                          value1,
                                                                          value2,
                                                                          match[0],
                                                                          match[1]
                                                                        ])));
                                                    }));
                                      } else {
                                        return Curry._1(finish, Jest.fail("should be more steps"));
                                      }
                                    }));
                      } else {
                        return Curry._1(finish, Jest.fail("should be more steps"));
                      }
                    }));
      }));

exports.Task = Task;
exports.Effect = Effect;
/*  Not a pure module */
