// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");

function withDefault($$default, param) {
  if (param !== undefined) {
    return Caml_option.valFromOption(param);
  } else {
    return $$default;
  }
}

function map(f, param) {
  if (param !== undefined) {
    return Caml_option.some(Curry._1(f, Caml_option.valFromOption(param)));
  }
  
}

function andThen(f, param) {
  if (param !== undefined) {
    return Curry._1(f, Caml_option.valFromOption(param));
  }
  
}

var map2 = /* () */0;

var map3 = /* () */0;

var map4 = /* () */0;

var map5 = /* () */0;

exports.withDefault = withDefault;
exports.map = map;
exports.map2 = map2;
exports.map3 = map3;
exports.map4 = map4;
exports.map5 = map5;
exports.andThen = andThen;
/* No side effect */
