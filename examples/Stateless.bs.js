// Generated by BUCKLESCRIPT VERSION 4.0.18, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var RealmOld = require("../src/RealmOld.bs.js");

var Html = Curry._1(RealmOld.React[/* Html */0], /* module */[]);

function view(message, param) {
  return Curry._2(Html[/* div */3], /* [] */0, /* :: */[
              Curry._1(Html[/* text */2], message),
              /* [] */0
            ]);
}

function mount(at) {
  return RealmOld.React[/* mount */1](at)((function (param) {
                return /* () */0;
              }), (function (param, param$1) {
                return /* () */0;
              }), (function (param) {
                return view("hello", param);
              }));
}

exports.Html = Html;
exports.view = view;
exports.mount = mount;
/* Html Not a pure module */
