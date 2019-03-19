// Generated by BUCKLESCRIPT VERSION 4.0.18, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Random = require("bs-platform/lib/js/random.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var ReactDOMRe = require("reason-react/src/ReactDOMRe.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");

function make(f) {
  return f;
}

function run(receiver, task) {
  return Curry._1(task, receiver);
}

function map(f, task, resolve) {
  return Curry._1(task, (function (a) {
                return Curry._1(resolve, Curry._1(f, a));
              }));
}

function randomInt(l, h, f) {
  return Curry._1(f, Random.$$int(h) + l | 0);
}

var Task = /* module */[
  /* make */make,
  /* run */run,
  /* map */map,
  /* randomInt */randomInt
];

function make$1(updater) {
  return (function (resolve) {
      return Curry._1(resolve, updater);
    });
}

function fromTask(task) {
  return task;
}

function run$1(receiver, model, command) {
  return Curry._1(command, (function (updater) {
                return Curry._1(receiver, Curry._1(updater, model));
              }));
}

function map$1(getter, setter, command) {
  return (function (param) {
      var f = function (updater, b) {
        return Curry._2(setter, b, Curry._1(updater, Curry._1(getter, b)));
      };
      return Curry._1(command, (function (a) {
                    return Curry._1(param, Curry._1(f, a));
                  }));
    });
}

var Cmd = /* module */[
  /* make */make$1,
  /* fromTask */fromTask,
  /* run */run$1,
  /* map */map$1
];

function _log(value) {
  console.log(JSON.stringify(value, null, 2));
  return /* () */0;
}

function run$2(mount, render, init, $staropt$star, view, arg) {
  var update = $staropt$star !== undefined ? $staropt$star : (function (x) {
        return x;
      });
  var model = /* record */[/* contents */Curry._1(init, arg)];
  var dispatch = function (action) {
    return run$1((function (newModel) {
                  _log(newModel);
                  model[0] = newModel;
                  return Curry._1(render, Curry._2(view, model[0], dispatch));
                }), model[0], Curry._1(update, action));
  };
  return Curry._1(mount, Curry._2(view, model[0], dispatch));
}

function map$2(getter, setter, element, dispatch) {
  return Curry._1(element, (function (command) {
                return Curry._1(dispatch, map$1(getter, setter, command));
              }));
}

function mountHtml(at) {
  var render = function (component) {
    return ReactDOMRe.renderToElementWithId(component, at);
  };
  return (function (param, param$1, param$2, param$3) {
      return run$2(render, render, param, param$1, param$2, param$3);
    });
}

function MakeHtml(T) {
  var className = function (name) {
    return /* Raw */Block.__(0, [
              "className",
              name
            ]);
  };
  var autofocus = function (value) {
    return /* Raw */Block.__(0, [
              "autoFocus",
              value
            ]);
  };
  var hidden = function (value) {
    return /* Raw */Block.__(0, [
              "hidden",
              value
            ]);
  };
  var name = function (name$1) {
    return /* Raw */Block.__(0, [
              "name",
              name$1
            ]);
  };
  var onClick = function (command) {
    return /* Event */Block.__(1, [
              "onClick",
              (function (param) {
                  return command;
                })
            ]);
  };
  var onDoubleClick = function (command) {
    return /* Event */Block.__(1, [
              "onDoubleClick",
              (function (param) {
                  return command;
                })
            ]);
  };
  var onChange = function (command) {
    return /* Event */Block.__(1, [
              "onChange",
              (function (param) {
                  return command;
                })
            ]);
  };
  var onBlur = function (command) {
    return /* Event */Block.__(1, [
              "onBlur",
              (function (param) {
                  return command;
                })
            ]);
  };
  var onInput = function (callback) {
    return /* Event */Block.__(1, [
              "onInput",
              (function ($$event) {
                  return Curry._1(callback, $$event.target.value);
                })
            ]);
  };
  var onKeyDown = function (callback) {
    return /* Event */Block.__(1, [
              "onKeyDown",
              (function ($$event) {
                  return Curry._1(callback, $$event.keyCode);
                })
            ]);
  };
  var Attr = /* module */[
    /* className */className,
    /* autofocus */autofocus,
    /* hidden */hidden,
    /* name */name,
    /* onClick */onClick,
    /* onDoubleClick */onDoubleClick,
    /* onChange */onChange,
    /* onBlur */onBlur,
    /* onInput */onInput,
    /* onKeyDown */onKeyDown
  ];
  var _element = function (elementName, $staropt$star, $staropt$star$1, $staropt$star$2, children, dispatch) {
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
    var attrs$2 = List.fold_left((function (attrs, param) {
            if (param.tag) {
              var callback = param[1];
              attrs[param[0]] = (function ($$event) {
                  return Curry._1(dispatch, Curry._1(callback, $$event));
                });
              return attrs;
            } else {
              attrs[param[0]] = param[1];
              return attrs;
            }
          }), { }, attrs$1);
    return ReasonReact.createDomElement(elementName, attrs$2, $$Array.of_list(List.map((function (el) {
                          return Curry._1(el, dispatch);
                        }), children)));
  };
  var $$null$1 = function (_dispatch) {
    return null;
  };
  var text = function (text$1, _dispatch) {
    return text$1;
  };
  var button = function (param, param$1, param$2, param$3, param$4) {
    return _element("button", param, param$1, param$2, param$3, param$4);
  };
  var footer = function (param, param$1, param$2, param$3, param$4) {
    return _element("footer", param, param$1, param$2, param$3, param$4);
  };
  var div = function (param, param$1, param$2, param$3, param$4) {
    return _element("div", param, param$1, param$2, param$3, param$4);
  };
  var header = function (param, param$1, param$2, param$3, param$4) {
    return _element("header", param, param$1, param$2, param$3, param$4);
  };
  var h1 = function (param, param$1, param$2, param$3, param$4) {
    return _element("h1", param, param$1, param$2, param$3, param$4);
  };
  var section = function (param, param$1, param$2, param$3, param$4) {
    return _element("section", param, param$1, param$2, param$3, param$4);
  };
  var span = function (param, param$1, param$2, param$3, param$4) {
    return _element("span", param, param$1, param$2, param$3, param$4);
  };
  var ul = function (param, param$1, param$2, param$3, param$4) {
    return _element("ul", param, param$1, param$2, param$3, param$4);
  };
  var li = function (param, param$1, param$2, param$3, param$4) {
    return _element("li", param, param$1, param$2, param$3, param$4);
  };
  var strong = function (param, param$1, param$2, param$3, param$4) {
    return _element("strong", param, param$1, param$2, param$3, param$4);
  };
  var p = function (param, param$1, param$2, param$3, param$4) {
    return _element("p", param, param$1, param$2, param$3, param$4);
  };
  var a = function ($staropt$star, $staropt$star$1) {
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
        return (function (param$2, param$3) {
            var param$4 = param;
            var param$5 = param$1;
            var param$6 = arg;
            var param$7 = param$2;
            var param$8 = param$3;
            return _element("a", param$4, param$5, param$6, param$7, param$8);
          });
      });
  };
  var label = function ($staropt$star, $staropt$star$1) {
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
        return (function (param$2, param$3) {
            var param$4 = param;
            var param$5 = param$1;
            var param$6 = arg;
            var param$7 = param$2;
            var param$8 = param$3;
            return _element("label", param$4, param$5, param$6, param$7, param$8);
          });
      });
  };
  var input = function ($staropt$star, id, className, $staropt$star$1, value) {
    var placeholder = $staropt$star !== undefined ? $staropt$star : "";
    var attrs = $staropt$star$1 !== undefined ? $staropt$star$1 : /* [] */0;
    if (value[0] >= 936573133) {
      var partial_arg = Pervasives.$at(/* :: */[
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
          ], attrs);
      return (function (param) {
          return _element("input", id, className, partial_arg, /* [] */0, param);
        });
    } else {
      var partial_arg$1 = Pervasives.$at(/* :: */[
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
          ], attrs);
      return (function (param) {
          return _element("input", id, className, partial_arg$1, /* [] */0, param);
        });
    }
  };
  return /* module */[
          /* Attr */Attr,
          /* _element */_element,
          /* null */$$null$1,
          /* text */text,
          /* button */button,
          /* footer */footer,
          /* div */div,
          /* header */header,
          /* h1 */h1,
          /* section */section,
          /* span */span,
          /* ul */ul,
          /* li */li,
          /* strong */strong,
          /* p */p,
          /* a */a,
          /* label */label,
          /* input */input
        ];
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

function onClick(command) {
  return /* Event */Block.__(1, [
            "onClick",
            (function (param) {
                return command;
              })
          ]);
}

function onDoubleClick(command) {
  return /* Event */Block.__(1, [
            "onDoubleClick",
            (function (param) {
                return command;
              })
          ]);
}

function onChange(command) {
  return /* Event */Block.__(1, [
            "onChange",
            (function (param) {
                return command;
              })
          ]);
}

function onBlur(command) {
  return /* Event */Block.__(1, [
            "onBlur",
            (function (param) {
                return command;
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

var Attr = /* module */[
  /* className */className,
  /* autofocus */autofocus,
  /* hidden */hidden,
  /* name */name,
  /* onClick */onClick,
  /* onDoubleClick */onDoubleClick,
  /* onChange */onChange,
  /* onBlur */onBlur,
  /* onInput */onInput,
  /* onKeyDown */onKeyDown
];

function _element(elementName, $staropt$star, $staropt$star$1, $staropt$star$2, children, dispatch) {
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
  var attrs$2 = List.fold_left((function (attrs, param) {
          if (param.tag) {
            var callback = param[1];
            attrs[param[0]] = (function ($$event) {
                return Curry._1(dispatch, Curry._1(callback, $$event));
              });
            return attrs;
          } else {
            attrs[param[0]] = param[1];
            return attrs;
          }
        }), { }, attrs$1);
  return ReasonReact.createDomElement(elementName, attrs$2, $$Array.of_list(List.map((function (el) {
                        return Curry._1(el, dispatch);
                      }), children)));
}

function $$null$1(_dispatch) {
  return null;
}

function text(text$1, _dispatch) {
  return text$1;
}

function button(param, param$1, param$2, param$3, param$4) {
  return _element("button", param, param$1, param$2, param$3, param$4);
}

function footer(param, param$1, param$2, param$3, param$4) {
  return _element("footer", param, param$1, param$2, param$3, param$4);
}

function div(param, param$1, param$2, param$3, param$4) {
  return _element("div", param, param$1, param$2, param$3, param$4);
}

function header(param, param$1, param$2, param$3, param$4) {
  return _element("header", param, param$1, param$2, param$3, param$4);
}

function h1(param, param$1, param$2, param$3, param$4) {
  return _element("h1", param, param$1, param$2, param$3, param$4);
}

function section(param, param$1, param$2, param$3, param$4) {
  return _element("section", param, param$1, param$2, param$3, param$4);
}

function span(param, param$1, param$2, param$3, param$4) {
  return _element("span", param, param$1, param$2, param$3, param$4);
}

function ul(param, param$1, param$2, param$3, param$4) {
  return _element("ul", param, param$1, param$2, param$3, param$4);
}

function li(param, param$1, param$2, param$3, param$4) {
  return _element("li", param, param$1, param$2, param$3, param$4);
}

function strong(param, param$1, param$2, param$3, param$4) {
  return _element("strong", param, param$1, param$2, param$3, param$4);
}

function p(param, param$1, param$2, param$3, param$4) {
  return _element("p", param, param$1, param$2, param$3, param$4);
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
      return (function (param$2, param$3) {
          var param$4 = param;
          var param$5 = param$1;
          var param$6 = arg;
          var param$7 = param$2;
          var param$8 = param$3;
          return _element("a", param$4, param$5, param$6, param$7, param$8);
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
      return (function (param$2, param$3) {
          var param$4 = param;
          var param$5 = param$1;
          var param$6 = arg;
          var param$7 = param$2;
          var param$8 = param$3;
          return _element("label", param$4, param$5, param$6, param$7, param$8);
        });
    });
}

function input($staropt$star, id, className, $staropt$star$1, value) {
  var placeholder = $staropt$star !== undefined ? $staropt$star : "";
  var attrs = $staropt$star$1 !== undefined ? $staropt$star$1 : /* [] */0;
  if (value[0] >= 936573133) {
    var partial_arg = Pervasives.$at(/* :: */[
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
        ], attrs);
    return (function (param) {
        return _element("input", id, className, partial_arg, /* [] */0, param);
      });
  } else {
    var partial_arg$1 = Pervasives.$at(/* :: */[
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
        ], attrs);
    return (function (param) {
        return _element("input", id, className, partial_arg$1, /* [] */0, param);
      });
  }
}

var Html = /* module */[
  /* Attr */Attr,
  /* _element */_element,
  /* null */$$null$1,
  /* text */text,
  /* button */button,
  /* footer */footer,
  /* div */div,
  /* header */header,
  /* h1 */h1,
  /* section */section,
  /* span */span,
  /* ul */ul,
  /* li */li,
  /* strong */strong,
  /* p */p,
  /* a */a,
  /* label */label,
  /* input */input
];

var Core = 0;

exports.Task = Task;
exports.Cmd = Cmd;
exports._log = _log;
exports.run = run$2;
exports.map = map$2;
exports.mountHtml = mountHtml;
exports.MakeHtml = MakeHtml;
exports.Html = Html;
exports.Core = Core;
/* ReactDOMRe Not a pure module */
