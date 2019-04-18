// Generated by BUCKLESCRIPT VERSION 5.0.2, PLEASE EDIT WITH CARE
'use strict';

var Clock = require("./Clock.bs.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Stateful = require("./Stateful.bs.js");
var ManyInput = require("./ManyInput.bs.js");
var Composition = require("./Composition.bs.js");
var CustomAction = require("./CustomAction.bs.js");
var Old_Stateful = require("./old/Old_Stateful.bs.js");
var Old_Stateless = require("./old/Old_Stateless.bs.js");
var FormValidation = require("./FormValidation.bs.js");
var Old_Composition = require("./old/Old_Composition.bs.js");
var ConcurrentEffect = require("./ConcurrentEffect.bs.js");
var Old_ReactInterop = require("./old/Old_ReactInterop.bs.js");

Curry._1(Stateful.App[/* mount */0], "stateful");

Curry._1(Composition.App[/* mount */0], "composition");

Curry._1(Clock.App[/* mount */0], "clock");

Curry._1(ManyInput.App[/* mount */0], "many-input");

Curry._1(FormValidation.App[/* mount */0], "form-validation");

Curry._1(CustomAction.App[/* mount */0], "custom-action");

Curry._1(ConcurrentEffect.App[/* mount */0], "concurrent-effect");

Old_Stateless.mount("old-stateless");

Old_Stateful.mount("old-stateful");

Old_ReactInterop.mount("old-react-interop");

Old_Composition.mount("old-composition");

/*  Not a pure module */
