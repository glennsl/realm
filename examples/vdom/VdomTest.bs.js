// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Vdom = require("../../src/Vdom.bs.js");

var oldNode = Vdom.$$Node[/* element */2](undefined, "strong", /* :: */[
      /* Attribute */[Vdom.Attribute[/* make */0](undefined, "class", "test-class")],
      /* [] */0
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

var node = Vdom.$$Node[/* element */2](undefined, "strong", /* :: */[
      /* Attribute */[Vdom.Attribute[/* make */0](undefined, "class", "test-class")],
      /* [] */0
    ], /* :: */[
      Vdom.$$Node[/* text */1]("Hello"),
      /* :: */[
        Vdom.$$Node[/* text */1]("!"),
        /* :: */[
          Vdom.$$Node[/* element */2](undefined, "em", /* [] */0, /* :: */[
                Vdom.$$Node[/* text */1](" world"),
                /* [] */0
              ]),
          /* [] */0
        ]
      ]
    ]);

var domNode = Vdom.render(node, "root");

List.iter((function (prim) {
        console.log(prim);
        return /* () */0;
      }), List.map(Vdom.pp_patch, Vdom.diff(domNode, oldNode, node)));

exports.oldNode = oldNode;
exports.node = node;
exports.domNode = domNode;
/* oldNode Not a pure module */
