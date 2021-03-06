// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Realm = require("../../src/Realm.bs.js");

function toggle(entry) {
  return Curry._1(Realm.Core[/* Effect */15][/* update */2], (function (model) {
                return /* record */[
                        /* entries */Curry._2(Realm.Core[/* List */3][/* map */4], (function (it) {
                                var match = it[/* id */0] === entry[/* id */0];
                                if (match) {
                                  return /* record */[
                                          /* id */entry[/* id */0],
                                          /* title */entry[/* title */1],
                                          /* completed */!entry[/* completed */2],
                                          /* editing */entry[/* editing */3],
                                          /* created */entry[/* created */4]
                                        ];
                                } else {
                                  return it;
                                }
                              }), model[/* entries */0]),
                        /* visibility */model[/* visibility */1]
                      ];
              }));
}

function edit(entry, editing) {
  return Curry._1(Realm.Core[/* Effect */15][/* update */2], (function (model) {
                return /* record */[
                        /* entries */Curry._2(Realm.Core[/* List */3][/* map */4], (function (it) {
                                var match = it[/* id */0] === entry[/* id */0];
                                if (match) {
                                  return /* record */[
                                          /* id */entry[/* id */0],
                                          /* title */entry[/* title */1],
                                          /* completed */entry[/* completed */2],
                                          /* editing */editing,
                                          /* created */entry[/* created */4]
                                        ];
                                } else {
                                  return it;
                                }
                              }), model[/* entries */0]),
                        /* visibility */model[/* visibility */1]
                      ];
              }));
}

function remove(entry) {
  return Curry._1(Realm.Core[/* Effect */15][/* update */2], (function (model) {
                return /* record */[
                        /* entries */Curry._2(Realm.Core[/* List */3][/* filter */8], (function (it) {
                                return it[/* id */0] !== entry[/* id */0];
                              }), model[/* entries */0]),
                        /* visibility */model[/* visibility */1]
                      ];
              }));
}

var clearCompleted = Curry._1(Realm.Core[/* Effect */15][/* update */2], (function (model) {
        return /* record */[
                /* entries */Curry._2(Realm.Core[/* List */3][/* filter */8], (function (it) {
                        return !it[/* completed */2];
                      }), model[/* entries */0]),
                /* visibility */model[/* visibility */1]
              ];
      }));

function changeVisibility(visibility) {
  return Curry._1(Realm.Core[/* Effect */15][/* update */2], (function (model) {
                return /* record */[
                        /* entries */model[/* entries */0],
                        /* visibility */visibility
                      ];
              }));
}

exports.toggle = toggle;
exports.edit = edit;
exports.remove = remove;
exports.clearCompleted = clearCompleted;
exports.changeVisibility = changeVisibility;
/* clearCompleted Not a pure module */
