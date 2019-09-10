// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Vdom = require("../../src/Vdom.bs.js");
var Block = require("bs-platform/lib/js/block.js");

function log(patches) {
  console.log("--");
  return List.iter((function (prim) {
                console.log(prim);
                return /* () */0;
              }), List.map(Vdom.pp_patch, patches));
}

var domNode = document.getElementById("root");

var current = /* record */[/* contents */Vdom.$$Node[/* text */1]("")];

function render(state) {
  if (state) {
    return Vdom.$$Node[/* element */2](undefined, "strong", /* :: */[
                /* Attribute */Block.__(0, [Vdom.Attribute[/* make */0](undefined, "class", "red")]),
                /* :: */[
                  /* Event */Block.__(1, [
                      "click",
                      (function (param) {
                          return onClick(state, param);
                        })
                    ]),
                  /* [] */0
                ]
              ], /* :: */[
                Vdom.$$Node[/* text */1]("Hello"),
                /* :: */[
                  Vdom.$$Node[/* element */2](undefined, "em", /* [] */0, /* :: */[
                        Vdom.$$Node[/* text */1](" world"),
                        /* [] */0
                      ]),
                  /* [] */0
                ]
              ]);
  } else {
    return Vdom.$$Node[/* element */2](undefined, "strong", /* :: */[
                /* Attribute */Block.__(0, [Vdom.Attribute[/* make */0](undefined, "class", "blue")]),
                /* :: */[
                  /* Event */Block.__(1, [
                      "click",
                      (function (param) {
                          return onClick(state, param);
                        })
                    ]),
                  /* [] */0
                ]
              ], /* :: */[
                Vdom.$$Node[/* text */1]("Hi"),
                /* :: */[
                  Vdom.$$Node[/* element */2](undefined, "em", /* [] */0, /* :: */[
                        Vdom.$$Node[/* text */1](" world"),
                        /* [] */0
                      ]),
                  /* :: */[
                    Vdom.$$Node[/* element */2](undefined, "span", /* [] */0, /* :: */[
                          Vdom.$$Node[/* text */1]("!"),
                          /* [] */0
                        ]),
                    /* [] */0
                  ]
                ]
              ]);
  }
}

function onClick(state, param) {
  var next = state ? render(/* First */0) : render(/* Second */1);
  var patches = Vdom.diff(domNode, current[0], next);
  log(patches);
  Vdom.patch(patches);
  current[0] = next;
  return /* () */0;
}

current[0] = render(/* First */0);

domNode.appendChild(Vdom.render(current[0]));

exports.log = log;
exports.domNode = domNode;
exports.current = current;
exports.render = render;
exports.onClick = onClick;
/* domNode Not a pure module */
