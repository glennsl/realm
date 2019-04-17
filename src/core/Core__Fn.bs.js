// Generated by BUCKLESCRIPT VERSION 5.0.2, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");

function id(x) {
  return x;
}

function always(x, param) {
  return x;
}

function never(x) {
  while(true) {
    continue ;
  };
}

function tap(f, x) {
  Curry._1(f, x);
  return x;
}

function $great$great(f, g, x) {
  return Curry._1(g, Curry._1(f, x));
}

function $less$less(f, g, x) {
  return Curry._1(f, Curry._1(g, x));
}

function $pipe$great(prim, prim$1) {
  return Curry._1(prim$1, prim);
}

function $less$pipe(prim, prim$1) {
  return Curry._1(prim, prim$1);
}

exports.id = id;
exports.always = always;
exports.never = never;
exports.tap = tap;
exports.$great$great = $great$great;
exports.$less$less = $less$less;
exports.$pipe$great = $pipe$great;
exports.$less$pipe = $less$pipe;
/* No side effect */
