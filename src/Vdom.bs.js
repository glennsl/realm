// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Block = require("bs-platform/lib/js/block.js");

function make(namespace, key, value) {
  return /* record */[
          /* namespace */namespace,
          /* key */key,
          /* value */value
        ];
}

var Attribute = /* module */[/* make */make];

function text(s) {
  return /* Text */Block.__(0, [s]);
}

function element(namespace, key, tagName, attributes, children) {
  return /* Element */Block.__(1, [/* record */[
              /* namespace */namespace,
              /* tagName */tagName,
              /* key */key,
              /* attributes */attributes,
              /* children */children
            ]]);
}

var $$Node = /* module */[
  /* text */text,
  /* element */element
];

function append(node, targetNode) {
  var domNode;
  if (node.tag) {
    var spec = node[0];
    var match = spec[/* namespace */0];
    var el = match !== undefined ? document.createElementNS(match, spec[/* tagName */1]) : document.createElement(spec[/* tagName */1]);
    List.iter((function (attr) {
            var match = attr[/* namespace */0];
            if (match !== undefined) {
              el.setAttribute(match, attr[/* key */1], attr[/* value */2]);
              return /* () */0;
            } else {
              el.setAttribute(attr[/* key */1], attr[/* value */2]);
              return /* () */0;
            }
          }), spec[/* attributes */3]);
    List.iter((function (child) {
            return append(child, el);
          }), spec[/* children */4]);
    domNode = el;
  } else {
    domNode = document.createTextNode(node[0]);
  }
  targetNode.appendChild(domNode);
  return /* () */0;
}

function render(node, targetId) {
  return append(node, document.getElementById(targetId));
}

exports.Attribute = Attribute;
exports.$$Node = $$Node;
exports.append = append;
exports.render = render;
/* No side effect */
