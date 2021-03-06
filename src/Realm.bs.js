// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var $$Array = require("bs-platform/lib/js/array.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Random = require("bs-platform/lib/js/random.js");
var Core__Fn = require("./core/Core__Fn.bs.js");
var Core__Int = require("./core/Core__Int.bs.js");
var Core__Bool = require("./core/Core__Bool.bs.js");
var Core__List = require("./core/Core__List.bs.js");
var Core__Math = require("./core/Core__Math.bs.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var ReactDOMRe = require("reason-react/src/ReactDOMRe.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");
var Core__Tuple = require("./core/Core__Tuple.bs.js");
var Realm__Core = require("./core/Realm__Core.bs.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");
var Core__Option = require("./core/Core__Option.bs.js");
var Core__Result = require("./core/Core__Result.bs.js");
var Core__String = require("./core/Core__String.bs.js");
var Belt_MapString = require("bs-platform/lib/js/belt_MapString.js");

function make(f) {
  return f;
}

function $$const(value, resolve) {
  return Curry._1(resolve, value);
}

function andThen(f, task, resolve) {
  return Curry._1(task, (function (a) {
                return Curry._2(f, a, resolve);
              }));
}

function map(f, task, resolve) {
  return Curry._1(task, (function (a) {
                return Curry._1(resolve, Curry._1(f, a));
              }));
}

function map2(f, taskA, taskB) {
  return Realm__Core.$pipe$great(taskA, (function (param, param$1) {
                var f$1 = function (a) {
                  return Realm__Core.$pipe$great(taskB, (function (param, param$1) {
                                var f$2 = function (b) {
                                  var partial_arg = Curry._2(f, a, b);
                                  return (function (param) {
                                      return Curry._1(param, partial_arg);
                                    });
                                };
                                return Curry._1(param, (function (a) {
                                              return Curry._2(f$2, a, param$1);
                                            }));
                              }));
                };
                return Curry._1(param, (function (a) {
                              return Curry._2(f$1, a, param$1);
                            }));
              }));
}

function all2(futureA, futureB) {
  var valueA = /* record */[/* contents */undefined];
  var valueB = /* record */[/* contents */undefined];
  return (function (resolve) {
      var tryResolve = function (param) {
        var match = valueA[0];
        var match$1 = valueB[0];
        if (match !== undefined && match$1 !== undefined) {
          return Curry._1(resolve, /* tuple */[
                      Caml_option.valFromOption(match),
                      Caml_option.valFromOption(match$1)
                    ]);
        } else {
          return /* () */0;
        }
      };
      Curry._1(futureA, (function (value) {
              valueA[0] = Caml_option.some(value);
              return tryResolve(/* () */0);
            }));
      return Curry._1(futureB, (function (value) {
                    valueB[0] = Caml_option.some(value);
                    return tryResolve(/* () */0);
                  }));
    });
}

function run(receiver, task) {
  return Curry._1(task, receiver);
}

function randomInt(l, h, f) {
  return Curry._1(f, Random.$$int(h) + l | 0);
}

var Future = /* module */[
  /* make */make,
  /* const */$$const,
  /* andThen */andThen,
  /* map */map,
  /* map2 */map2,
  /* all2 */all2,
  /* run */run,
  /* randomInt */randomInt
];

function $$const$1(value) {
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
                  return Realm__Core.$pipe$great(Curry._1(action, model), (function (param, param$1) {
                                return Curry._1(param, (function (a) {
                                              return Curry._1(param$1, Curry._1(mapper, a));
                                            }));
                              }));
                })]),
          /* [] */0
        ];
}

function andThen$1(last, param) {
  if (param) {
    var match = param[0];
    if (match.tag) {
      return /* :: */[
              /* Task */Block.__(1, [match[0]]),
              andThen$1(last, param[1])
            ];
    } else {
      return /* :: */[
              /* Update */Block.__(0, [match[0]]),
              andThen$1(last, param[1])
            ];
    }
  } else {
    return last;
  }
}

function map$1(get, set, param) {
  if (param) {
    var match = param[0];
    if (match.tag) {
      var f = match[0];
      return /* :: */[
              /* Task */Block.__(1, [(function (model) {
                      return Realm__Core.$pipe$great(Realm__Core.$pipe$great(Realm__Core.$pipe$great(model, get), f), (function (param, param$1) {
                                    var f = function (f$1, model) {
                                      return Realm__Core.$pipe$great(Realm__Core.$pipe$great(Realm__Core.$pipe$great(model, get), f$1), Curry._1(set, model));
                                    };
                                    return Curry._1(param, (function (a) {
                                                  return Curry._1(param$1, Curry._1(f, a));
                                                }));
                                  }));
                    })]),
              map$1(get, set, param[1])
            ];
    } else {
      var f$1 = match[0];
      return /* :: */[
              /* Update */Block.__(0, [(function (model) {
                      return Realm__Core.$pipe$great(Realm__Core.$pipe$great(Realm__Core.$pipe$great(model, get), f$1), Curry._1(set, model));
                    })]),
              map$1(get, set, param[1])
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
      var next = Realm__Core.$pipe$great(Curry._1(match[0], model), (function (param, param$1) {
              var f = function (f$prime) {
                return /* :: */[
                        /* Update */Block.__(0, [f$prime]),
                        rest
                      ];
              };
              return Curry._1(param, (function (a) {
                            return Curry._1(param$1, Curry._1(f, a));
                          }));
            }));
      return /* tuple */[
              undefined,
              next
            ];
    } else {
      var rest$1 = param[1];
      var f = match[0];
      if (rest$1) {
        return /* tuple */[
                Caml_option.some(Curry._1(f, model)),
                (function (param) {
                    return Curry._1(param, rest$1);
                  })
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

function make$1(id, action, spawner) {
  return /* record */[
          /* id */id,
          /* spawner */(function (dispatch) {
              return Curry._1(spawner, (function (value) {
                            return Realm__Core.$pipe$great(Realm__Core.$pipe$great(value, action), dispatch);
                          }));
            })
        ];
}

function unsub(unsubber) {
  return Curry._1(unsubber, /* () */0);
}

function now(resolve) {
  return Realm__Core.$pipe$great(Realm__Core.$pipe$great(Date.now(), (function (prim) {
                    return new Date(prim);
                  })), resolve);
}

function delay(ms) {
  return (function (resolve) {
      setTimeout(resolve, ms);
      return /* () */0;
    });
}

function every(id, ms, action) {
  return make$1(id, action, (function (callback) {
                var intervalId = setInterval(callback, ms);
                return (function (param) {
                    clearInterval(intervalId);
                    return /* () */0;
                  });
              }));
}

function toString(prim) {
  return prim.toTimeString();
}

var Time = /* module */[
  /* now */now,
  /* delay */delay,
  /* every */every,
  /* toString */toString
];

function run$1(mount, render, init, update, subs, view, arg) {
  return Curry._2(init, arg, (function (initialModel) {
                var activeSubs = /* record */[/* contents */Belt_MapString.empty];
                var model = /* record */[/* contents */initialModel];
                var updateSubs = function (param) {
                  var newSubs = Realm__Core.$pipe$great(Curry._1(subs, model[0]), (function (param) {
                          return Core__List.foldl((function (subs, sub) {
                                        return Belt_MapString.set(subs, sub[/* id */0], sub);
                                      }), Belt_MapString.empty, param);
                        }));
                  var spawns = Belt_MapString.keep(newSubs, (function (key, param) {
                          return !Belt_MapString.has(activeSubs[0], key);
                        }));
                  var existing = Belt_MapString.keep(activeSubs[0], (function (key, param) {
                          return Belt_MapString.has(newSubs, key);
                        }));
                  var kills = Belt_MapString.keep(activeSubs[0], (function (key, param) {
                          return !Belt_MapString.has(newSubs, key);
                        }));
                  Belt_MapString.forEach(kills, (function (param) {
                          return unsub;
                        }));
                  activeSubs[0] = Belt_MapString.reduce(spawns, existing, (function (subs, id, sub) {
                          return Belt_MapString.set(subs, id, Curry._1(sub[/* spawner */1], dispatch));
                        }));
                  console.log("updateSubs", activeSubs[0]);
                  return /* () */0;
                };
                var dispatch = function (action) {
                  var runEffect = function (effect) {
                    var match = step(model[0], effect);
                    var nextEffect = match[1];
                    var maybeModel = match[0];
                    if (maybeModel !== undefined) {
                      var newModel = Caml_option.valFromOption(maybeModel);
                      console.log("model updated", newModel);
                      model[0] = newModel;
                      updateSubs(/* () */0);
                      return Curry._1(render, Curry._2(view, model[0], dispatch));
                    } else if (nextEffect !== undefined) {
                      return Curry._1(nextEffect, runEffect);
                    } else {
                      return /* () */0;
                    }
                  };
                  return Realm__Core.$pipe$great(Realm__Core.$pipe$great(action, update), runEffect);
                };
                updateSubs(/* () */0);
                return Curry._1(mount, Curry._2(view, model[0], dispatch));
              }));
}

function className(name) {
  return /* Raw */Block.__(0, [
            "className",
            name
          ]);
}

function autofocus(value) {
  return /* Raw */Block.__(0, [
            "autoFocus",
            value
          ]);
}

function hidden(value) {
  return /* Raw */Block.__(0, [
            "hidden",
            value
          ]);
}

function name(name$1) {
  return /* Raw */Block.__(0, [
            "name",
            name$1
          ]);
}

function onClick(action) {
  return /* Event */Block.__(1, [
            "onClick",
            (function (param) {
                return action;
              })
          ]);
}

function onDoubleClick(action) {
  return /* Event */Block.__(1, [
            "onDoubleClick",
            (function (param) {
                return action;
              })
          ]);
}

function onChange(action) {
  return /* Event */Block.__(1, [
            "onChange",
            (function (param) {
                return action;
              })
          ]);
}

function onBlur(action) {
  return /* Event */Block.__(1, [
            "onBlur",
            (function (param) {
                return action;
              })
          ]);
}

function onInput(callback) {
  return /* Event */Block.__(1, [
            "onInput",
            (function ($$event) {
                return Curry._1(callback, $$event.target.value);
              })
          ]);
}

function onKeyDown(callback) {
  return /* Event */Block.__(1, [
            "onKeyDown",
            (function ($$event) {
                return Curry._1(callback, $$event.keyCode);
              })
          ]);
}

function toProps(dispatch, attrs) {
  var addProp = function (obj, attr) {
    if (attr.tag) {
      var callback = attr[1];
      obj[attr[0]] = (function ($$event) {
          return Curry._1(dispatch, Curry._1(callback, $$event));
        });
    } else {
      obj[attr[0]] = attr[1];
    }
    return obj;
  };
  return Core__List.foldl(addProp, { }, attrs);
}

function _element(tag, $staropt$star, $staropt$star$1, $staropt$star$2, children) {
  var id = $staropt$star !== undefined ? $staropt$star : "";
  var className = $staropt$star$1 !== undefined ? $staropt$star$1 : "";
  var attrs = $staropt$star$2 !== undefined ? $staropt$star$2 : /* [] */0;
  var attrs_000 = /* Raw */Block.__(0, [
      "id",
      id
    ]);
  var attrs_001 = /* :: */[
    /* Raw */Block.__(0, [
        "className",
        className
      ]),
    attrs
  ];
  var attrs$1 = /* :: */[
    attrs_000,
    attrs_001
  ];
  return (function (dispatch) {
      return ReasonReact.createDomElement(tag, toProps(dispatch, attrs$1), Realm__Core.$pipe$great(Realm__Core.$pipe$great(children, (function (param) {
                            return Core__List.map((function (el) {
                                          return Curry._1(el, dispatch);
                                        }), param);
                          })), $$Array.of_list));
    });
}

function $$null$1(_dispatch) {
  return null;
}

function text(text$1, _dispatch) {
  return text$1;
}

function button(id) {
  return (function (param, param$1, param$2) {
      return _element("button", id, param, param$1, param$2);
    });
}

function div(id) {
  return (function (param, param$1, param$2) {
      return _element("div", id, param, param$1, param$2);
    });
}

function footer(id) {
  return (function (param, param$1, param$2) {
      return _element("footer", id, param, param$1, param$2);
    });
}

function header(id) {
  return (function (param, param$1, param$2) {
      return _element("header", id, param, param$1, param$2);
    });
}

function h1(id) {
  return (function (param, param$1, param$2) {
      return _element("h1", id, param, param$1, param$2);
    });
}

function section(id) {
  return (function (param, param$1, param$2) {
      return _element("section", id, param, param$1, param$2);
    });
}

function span(id) {
  return (function (param, param$1, param$2) {
      return _element("span", id, param, param$1, param$2);
    });
}

function ul(id) {
  return (function (param, param$1, param$2) {
      return _element("ul", id, param, param$1, param$2);
    });
}

function li(id) {
  return (function (param, param$1, param$2) {
      return _element("li", id, param, param$1, param$2);
    });
}

function strong(id) {
  return (function (param, param$1, param$2) {
      return _element("strong", id, param, param$1, param$2);
    });
}

function p(id) {
  return (function (param, param$1, param$2) {
      return _element("p", id, param, param$1, param$2);
    });
}

function a($staropt$star, $staropt$star$1) {
  var href = $staropt$star !== undefined ? $staropt$star : "";
  var attrs = $staropt$star$1 !== undefined ? $staropt$star$1 : /* [] */0;
  var arg = Pervasives.$at(/* :: */[
        /* Raw */Block.__(0, [
            "href",
            href
          ]),
        /* [] */0
      ], attrs);
  return (function (param, param$1) {
      return (function (param$2) {
          var param$3 = param;
          var param$4 = param$1;
          var param$5 = arg;
          var param$6 = param$2;
          return _element("a", param$3, param$4, param$5, param$6);
        });
    });
}

function label($staropt$star, $staropt$star$1) {
  var for_ = $staropt$star !== undefined ? $staropt$star : "";
  var attrs = $staropt$star$1 !== undefined ? $staropt$star$1 : /* [] */0;
  var arg = Pervasives.$at(/* :: */[
        /* Raw */Block.__(0, [
            "htmlFor",
            for_
          ]),
        /* [] */0
      ], attrs);
  return (function (param, param$1) {
      return (function (param$2) {
          var param$3 = param;
          var param$4 = param$1;
          var param$5 = arg;
          var param$6 = param$2;
          return _element("label", param$3, param$4, param$5, param$6);
        });
    });
}

function input($staropt$star, value, id, className, $staropt$star$1, children) {
  var placeholder = $staropt$star !== undefined ? $staropt$star : "";
  var attrs = $staropt$star$1 !== undefined ? $staropt$star$1 : /* [] */0;
  if (value[0] >= 936573133) {
    return _element("input", id, className, Pervasives.$at(/* :: */[
                    /* Raw */Block.__(0, [
                        "placeholder",
                        placeholder
                      ]),
                    /* :: */[
                      /* Raw */Block.__(0, [
                          "value",
                          value[1]
                        ]),
                      /* [] */0
                    ]
                  ], attrs), children);
  } else {
    return _element("input", id, className, Pervasives.$at(/* :: */[
                    /* Raw */Block.__(0, [
                        "type",
                        "checkbox"
                      ]),
                    /* :: */[
                      /* Raw */Block.__(0, [
                          "checked",
                          value[1]
                        ]),
                      /* [] */0
                    ]
                  ], attrs), children);
  }
}

function map$2(f, element, dispatch) {
  return Curry._1(element, (function (param) {
                return Realm__Core.$great$great(f, dispatch, param);
              }));
}

function raw(f) {
  return f;
}

function App(Spec) {
  var mount = function (at) {
    var render = function (component) {
      return ReactDOMRe.renderToElementWithId(component, at);
    };
    return run$1(render, render, Spec[/* init */0], Spec[/* update */1], Spec[/* subs */2], Spec[/* view */3], /* () */0);
  };
  return /* module */[/* mount */mount];
}

function SimpleApp(Spec) {
  var init = Spec[0];
  var view = Spec[1];
  var init$1 = function (param) {
    var partial_arg = Curry._1(init, /* () */0);
    return (function (param) {
        return Curry._1(param, partial_arg);
      });
  };
  var update = function (x) {
    return x;
  };
  var subs = function (param) {
    return /* [] */0;
  };
  var mount = function (at) {
    var render = function (component) {
      return ReactDOMRe.renderToElementWithId(component, at);
    };
    return run$1(render, render, init$1, update, subs, view, /* () */0);
  };
  return /* module */[/* mount */mount];
}

var Core_000 = /* Core__Bool */[
  Core__Bool.not,
  Core__Bool.or_,
  Core__Bool.and_,
  Core__Bool.xor
];

var Core_001 = /* Core__Fn */[
  Core__Fn.id,
  Core__Fn.always,
  Core__Fn.never,
  Core__Fn.tap,
  Core__Fn.$great$great,
  Core__Fn.$less$less,
  Core__Fn.$pipe$great,
  Core__Fn.$less$pipe
];

var Core_002 = /* Core__Int */[
  Core__Int.add,
  Core__Int.subtract,
  Core__Int.multiply,
  Core__Int.divide,
  Core__Int.pow,
  Core__Int.toFloat,
  Core__Int.equals,
  Core__Int.notEquals,
  Core__Int.lessThan,
  Core__Int.greaterThan,
  Core__Int.lessThanOrEqual,
  Core__Int.greaterThanOrEqual,
  Core__Int.min,
  Core__Int.max,
  Core__Int.compare,
  Core__Int.modBy,
  Core__Int.remainderBy,
  Core__Int.negate,
  Core__Int.clamp,
  Core__Int.round,
  Core__Int.floor,
  Core__Int.ceiling,
  Core__Int.truncate,
  Core__Int.toInt,
  Core__Int.$$isNaN,
  Core__Int.$$isFinite
];

var Core_003 = /* Core__List */[
  Core__List.singleton,
  Core__List.repeat,
  Core__List.range,
  Core__List.cons,
  Core__List.map,
  Core__List.mapi,
  Core__List.foldl,
  Core__List.foldr,
  Core__List.filter,
  Core__List.filterMap,
  Core__List.length,
  Core__List.reverse,
  Core__List.has,
  Core__List.all,
  Core__List.any,
  Core__List.max,
  Core__List.min,
  Core__List.sum,
  Core__List.product,
  Core__List.append,
  Core__List.concat,
  Core__List.concatMap,
  Core__List.intersperse,
  Core__List.map2,
  Core__List.map3,
  Core__List.map4,
  Core__List.map5,
  Core__List.sort,
  Core__List.sortBy,
  Core__List.sortWith,
  Core__List.isEmpty,
  Core__List.head,
  Core__List.tail,
  Core__List.take,
  Core__List.drop,
  Core__List.partition,
  Core__List.unzip,
  Core__List.$plus$plus
];

var Core_004 = /* Core__Math */[
  Core__Math.sqrt,
  Core__Math.logBase,
  Core__Math.e,
  Core__Math.pi,
  Core__Math.cos,
  Core__Math.sin,
  Core__Math.tan,
  Core__Math.acos,
  Core__Math.asin,
  Core__Math.atan,
  Core__Math.atan2
];

var Core_005 = /* Core__Option */[
  Core__Option.withDefault,
  Core__Option.map,
  Core__Option.map2,
  Core__Option.map3,
  Core__Option.map4,
  Core__Option.map5,
  Core__Option.andThen
];

var Core_006 = /* Core__Result */[
  Core__Result.map,
  Core__Result.map2,
  Core__Result.map3,
  Core__Result.map4,
  Core__Result.map5,
  Core__Result.andThen,
  Core__Result.withDefault,
  Core__Result.toOption,
  Core__Result.fromOption,
  Core__Result.mapError
];

var Core_007 = /* Core__String */[
  Core__String.isEmpty,
  Core__String.length,
  Core__String.reverse,
  Core__String.repeat,
  Core__String.replace,
  Core__String.append,
  Core__String.concat,
  Core__String.split,
  Core__String.join,
  Core__String.words,
  Core__String.lines,
  Core__String.slice,
  Core__String.left,
  Core__String.right,
  Core__String.dropLeft,
  Core__String.dropRight,
  Core__String.contains,
  Core__String.startsWith,
  Core__String.endsWith,
  Core__String.indexes,
  Core__String.indices,
  Core__String.toInt,
  Core__String.fromInt,
  Core__String.toFloat,
  Core__String.fromFloat,
  Core__String.fromChar,
  Core__String.cons,
  Core__String.uncons,
  Core__String.toList,
  Core__String.fromList,
  Core__String.toUpper,
  Core__String.toLower,
  Core__String.pad,
  Core__String.padLeft,
  Core__String.padRight,
  Core__String.trim,
  Core__String.trimLeft,
  Core__String.trimRight,
  Core__String.map,
  Core__String.filter,
  Core__String.reduce,
  Core__String.reduceRight,
  Core__String.any,
  Core__String.all
];

var Core_008 = /* Core__Tuple */[
  Core__Tuple.pair,
  Core__Tuple.first,
  Core__Tuple.second,
  Core__Tuple.mapFirst,
  Core__Tuple.mapSecond,
  Core__Tuple.mapBoth
];

var Core_015 = [
  /* [] */0,
  $$const$1,
  update,
  do_,
  andThen$1,
  map$1
];

var Core_016 = [make$1];

var Core = [
  Core_000,
  Core_001,
  Core_002,
  Core_003,
  Core_004,
  Core_005,
  Core_006,
  Core_007,
  Core_008,
  Realm__Core.$great$great,
  Realm__Core.$less$less,
  Realm__Core.$pipe$great,
  Realm__Core.$less$pipe,
  Realm__Core.$plus$plus,
  Future,
  Core_015,
  Core_016,
  Time
];

var React_000 = [
  [
    className,
    autofocus,
    hidden,
    name,
    onClick,
    onDoubleClick,
    onChange,
    onBlur,
    onInput,
    onKeyDown
  ],
  $$null$1,
  text,
  button,
  div,
  footer,
  header,
  h1,
  section,
  span,
  ul,
  li,
  strong,
  p,
  a,
  label,
  input,
  map$2,
  raw
];

var React = [
  React_000,
  App,
  SimpleApp
];

exports.Core = Core;
exports.React = React;
/* ReactDOMRe Not a pure module */
