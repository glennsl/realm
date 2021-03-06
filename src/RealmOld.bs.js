// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var ReactDOMRe = require("reason-react/src/ReactDOMRe.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");

function Core(Config) {
  var run = function (mount, render, init, update, view) {
    var model = /* record */[/* contents */Curry._1(init, /* () */0)];
    var dispatch = function (msg) {
      model[0] = Curry._2(update, msg, model[0]);
      return Curry._1(render, Curry._2(view, model[0], dispatch));
    };
    return Curry._1(mount, Curry._2(view, model[0], dispatch));
  };
  var map = function (f, element, dispatch) {
    return Curry._1(element, (function (msg) {
                  return Curry._1(dispatch, Curry._1(f, msg));
                }));
  };
  return /* module */[
          /* run */run,
          /* map */map
        ];
}

function map(f, element, dispatch) {
  return Curry._1(element, (function (msg) {
                return Curry._1(dispatch, Curry._1(f, msg));
              }));
}

function mount(at) {
  var render = function (component) {
    return ReactDOMRe.renderToElementWithId(component, at);
  };
  return (function (param, param$1, param$2) {
      var mount = render;
      var render$1 = render;
      var init = param;
      var update = param$1;
      var view = param$2;
      var model = /* record */[/* contents */Curry._1(init, /* () */0)];
      var dispatch = function (msg) {
        model[0] = Curry._2(update, msg, model[0]);
        return Curry._1(render$1, Curry._2(view, model[0], dispatch));
      };
      return Curry._1(mount, Curry._2(view, model[0], dispatch));
    });
}

function React_000(funarg) {
  var onClick = function (msg) {
    return /* Event */Block.__(1, [
              "onClick",
              msg
            ]);
  };
  var _element = function (elementName, props, children, dispatch) {
    var props$1 = List.fold_left((function (props, param) {
            if (param.tag) {
              var msg = param[1];
              props[param[0]] = (function (_event) {
                  return Curry._1(dispatch, msg);
                });
              return props;
            } else {
              props[param[0]] = param[1];
              return props;
            }
          }), { }, props);
    return ReasonReact.createDomElement(elementName, props$1, $$Array.of_list(List.map((function (el) {
                          return Curry._1(el, dispatch);
                        }), children)));
  };
  var $$null$1 = function (_dispatch) {
    return null;
  };
  var text = function (text$1, _dispatch) {
    return text$1;
  };
  var div = function (param, param$1, param$2) {
    return _element("div", param, param$1, param$2);
  };
  var button = function (param, param$1, param$2) {
    return _element("button", param, param$1, param$2);
  };
  var fromReact = function (f) {
    return f;
  };
  return [
          onClick,
          $$null$1,
          text,
          div,
          button,
          fromReact
        ];
}

var React = /* module */[
  React_000,
  /* mount */mount,
  /* map */map
];

exports.Core = Core;
exports.React = React;
/* ReactDOMRe Not a pure module */
