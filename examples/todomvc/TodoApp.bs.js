// Generated by BUCKLESCRIPT VERSION 5.0.2, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Realm = require("../../src/Realm.bs.js");
var Header = require("./Header.bs.js");
var TodoFooter = require("./TodoFooter.bs.js");
var MainSection = require("./MainSection.bs.js");

function view(entries, visibility) {
  return Curry._4(Realm.React[/* Html */0][/* div */4], undefined, "todoapp", undefined, /* :: */[
              Header.view(/* () */0),
              /* :: */[
                MainSection.view(entries, visibility),
                /* :: */[
                  TodoFooter.view(entries, visibility),
                  /* [] */0
                ]
              ]
            ]);
}

exports.view = view;
/* Realm Not a pure module */
