// Generated by BUCKLESCRIPT VERSION 4.0.18, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Model = require("./Model.bs.js");
var TodoList = require("./TodoList.bs.js");

function toggleAllCheckbox(param) {
  return Curry._6(Model.Html[/* input */16], undefined, /* `Checkbox */[
              111644259,
              false
            ], undefined, "toggle-all", undefined, /* [] */0);
}

function view(entries, visibility) {
  return Curry._4(Model.Html[/* section */8], undefined, undefined, undefined, /* :: */[
              toggleAllCheckbox(entries),
              /* :: */[
                TodoList.view(entries, visibility),
                /* [] */0
              ]
            ]);
}

exports.toggleAllCheckbox = toggleAllCheckbox;
exports.view = view;
/* Model Not a pure module */
