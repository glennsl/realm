// Generated by BUCKLESCRIPT VERSION 4.0.18, PLEASE EDIT WITH CARE
'use strict';

var RealmNoUpdate = require("../../src/RealmNoUpdate.bs.js");

function newId(param) {
  return ('xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => (c === 'x' ? Math.random()*16|0 : (((Math.random()*16|0)&0x3)|0x8)).toString(16)));
}

function make(title) {
  return /* record */[
          /* id */newId(/* () */0),
          /* title */title,
          /* completed */false,
          /* editing */false,
          /* created */new Date()
        ];
}

var Todo = /* module */[
  /* newId */newId,
  /* make */make
];

var Html = RealmNoUpdate.MakeHtml(/* module */[]);

exports.Todo = Todo;
exports.Html = Html;
/* Html Not a pure module */