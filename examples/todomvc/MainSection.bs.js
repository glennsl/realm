// Generated by BUCKLESCRIPT VERSION 4.0.18, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Model = require("./Model.bs.js");
var TodoList = require("./TodoList.bs.js");

function toggleAllCheckbox(param) {
  var func = Model.Html[/* input */17];
  return (function (param, param$1, param$2) {
      return Curry._5(func, param, param$1, "toggle-all", param$2, /* `Checkbox */[
                  111644259,
                  false
                ]);
    });
}

function view(entries, visibility) {
  var arg = toggleAllCheckbox(entries);
  return Curry._4(Model.Html[/* section */9], undefined, undefined, undefined, /* :: */[
              (function (eta) {
                  return Curry._4(arg, undefined, undefined, undefined, eta);
                }),
              /* :: */[
                TodoList.view(entries, visibility),
                /* [] */0
              ]
            ]);
}

exports.toggleAllCheckbox = toggleAllCheckbox;
exports.view = view;
/* Model Not a pure module */