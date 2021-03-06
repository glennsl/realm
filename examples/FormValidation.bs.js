// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Realm = require("../src/Realm.bs.js");

function init(validate, value) {
  return /* record */[
          /* validate */validate,
          /* value */value,
          /* state */Curry._1(validate, value),
          /* initial */true
        ];
}

function get(field) {
  return field[/* value */1];
}

function set(field, value) {
  return /* record */[
          /* validate */field[/* validate */0],
          /* value */value,
          /* state */Curry._1(field[/* validate */0], value),
          /* initial */false
        ];
}

function touch(field) {
  return /* record */[
          /* validate */field[/* validate */0],
          /* value */field[/* value */1],
          /* state */field[/* state */2],
          /* initial */false
        ];
}

function error(field) {
  var match = field[/* state */2];
  if (match) {
    return match[0];
  }
  
}

function isValid(field) {
  return field[/* state */2] === /* Valid */0;
}

function isInitial(field) {
  return field[/* initial */3];
}

var Validation = /* module */[
  /* init */init,
  /* get */get,
  /* set */set,
  /* touch */touch,
  /* error */error,
  /* isValid */isValid,
  /* isInitial */isInitial
];

function isRequired(value) {
  if (value === "") {
    return /* Invalid */["Field is required"];
  } else {
    return /* Valid */0;
  }
}

function isValidEmail(value) {
  var re = (/^[a-zA-Z0-9.!#$%&'*+\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$/);
  if (re.test(value)) {
    return /* Valid */0;
  } else {
    return /* Invalid */["Not a valid e-mail address"];
  }
}

function isJson(value) {
  try {
    JSON.parse(value);
    return /* Valid */0;
  }
  catch (exn){
    return /* Invalid */["Not valid Json"];
  }
}

function isInteger(value) {
  var re = (/^-?[0-9]+$/);
  if (re.test(value)) {
    return /* Valid */0;
  } else {
    return /* Invalid */["Not an integer"];
  }
}

function validatedTextInput(placeholder, field) {
  var partial_arg = Realm.Core[/* Effect */15][/* const */1];
  var partial_arg$1 = Realm.Core[/* >> */9];
  var tmp;
  if (field[/* initial */3]) {
    tmp = Realm.React[/* Html */0][/* null */1];
  } else {
    var match = error(field);
    tmp = match !== undefined ? Curry._4(Realm.React[/* Html */0][/* span */9], undefined, undefined, undefined, /* :: */[
            Curry._1(Realm.React[/* Html */0][/* text */2], match),
            /* [] */0
          ]) : Realm.React[/* Html */0][/* null */1];
  }
  return Curry._4(Realm.React[/* Html */0][/* div */4], undefined, undefined, undefined, /* :: */[
              Curry._6(Realm.React[/* Html */0][/* input */16], placeholder, /* `Text */[
                    936573133,
                    field[/* value */1]
                  ], undefined, undefined, /* :: */[
                    Curry._1(Realm.React[/* Html */0][/* Attr */0][/* onInput */8], (function (param) {
                            return partial_arg$1((function (param) {
                                          return set(field, param);
                                        }), partial_arg, param);
                          })),
                    /* [] */0
                  ], /* [] */0),
              /* :: */[
                tmp,
                /* [] */0
              ]
            ]);
}

function init$1(param) {
  return /* record */[
          /* a */init(isRequired, ""),
          /* b */init(isValidEmail, ""),
          /* c */init(isJson, ""),
          /* d */init(isInteger, ""),
          /* submitMessage */""
        ];
}

var submit = Curry._1(Realm.Core[/* Effect */15][/* update */2], (function (model) {
        var errors = Curry._2(Realm.Core[/* List */3][/* filterMap */9], Realm.Core[/* Fn */1][/* id */0], /* :: */[
              error(model[/* a */0]),
              /* :: */[
                error(model[/* b */1]),
                /* :: */[
                  error(model[/* c */2]),
                  /* :: */[
                    error(model[/* d */3]),
                    /* [] */0
                  ]
                ]
              ]
            ]);
        if (errors === /* [] */0) {
          return /* record */[
                  /* a */model[/* a */0],
                  /* b */model[/* b */1],
                  /* c */model[/* c */2],
                  /* d */model[/* d */3],
                  /* submitMessage */"Success!"
                ];
        } else {
          return /* record */[
                  /* a */touch(model[/* a */0]),
                  /* b */touch(model[/* b */1]),
                  /* c */touch(model[/* c */2]),
                  /* d */touch(model[/* d */3]),
                  /* submitMessage */"Error!"
                ];
        }
      }));

function view(model) {
  return Curry._4(Realm.React[/* Html */0][/* div */4], undefined, undefined, undefined, /* :: */[
              Realm.Core[/* |> */11](validatedTextInput("Required", model[/* a */0]), Curry._1(Realm.React[/* Html */0][/* map */17], Curry._2(Realm.Core[/* Effect */15][/* map */5], (function (m) {
                              return m[/* a */0];
                            }), (function (m, v) {
                              return /* record */[
                                      /* a */v,
                                      /* b */m[/* b */1],
                                      /* c */m[/* c */2],
                                      /* d */m[/* d */3],
                                      /* submitMessage */m[/* submitMessage */4]
                                    ];
                            })))),
              /* :: */[
                Realm.Core[/* |> */11](validatedTextInput("E-mail address", model[/* b */1]), Curry._1(Realm.React[/* Html */0][/* map */17], Curry._2(Realm.Core[/* Effect */15][/* map */5], (function (m) {
                                return m[/* b */1];
                              }), (function (m, v) {
                                return /* record */[
                                        /* a */m[/* a */0],
                                        /* b */v,
                                        /* c */m[/* c */2],
                                        /* d */m[/* d */3],
                                        /* submitMessage */m[/* submitMessage */4]
                                      ];
                              })))),
                /* :: */[
                  Realm.Core[/* |> */11](validatedTextInput("JSON", model[/* c */2]), Curry._1(Realm.React[/* Html */0][/* map */17], Curry._2(Realm.Core[/* Effect */15][/* map */5], (function (m) {
                                  return m[/* c */2];
                                }), (function (m, v) {
                                  return /* record */[
                                          /* a */m[/* a */0],
                                          /* b */m[/* b */1],
                                          /* c */v,
                                          /* d */m[/* d */3],
                                          /* submitMessage */m[/* submitMessage */4]
                                        ];
                                })))),
                  /* :: */[
                    Realm.Core[/* |> */11](validatedTextInput("Whole number", model[/* d */3]), Curry._1(Realm.React[/* Html */0][/* map */17], Curry._2(Realm.Core[/* Effect */15][/* map */5], (function (m) {
                                    return m[/* d */3];
                                  }), (function (m, v) {
                                    return /* record */[
                                            /* a */m[/* a */0],
                                            /* b */m[/* b */1],
                                            /* c */m[/* c */2],
                                            /* d */v,
                                            /* submitMessage */m[/* submitMessage */4]
                                          ];
                                  })))),
                    /* :: */[
                      Curry._4(Realm.React[/* Html */0][/* button */3], undefined, undefined, /* :: */[
                            Curry._1(Realm.React[/* Html */0][/* Attr */0][/* onClick */4], submit),
                            /* [] */0
                          ], /* :: */[
                            Curry._1(Realm.React[/* Html */0][/* text */2], "Submit"),
                            /* [] */0
                          ]),
                      /* :: */[
                        Curry._1(Realm.React[/* Html */0][/* text */2], model[/* submitMessage */4]),
                        /* [] */0
                      ]
                    ]
                  ]
                ]
              ]
            ]);
}

var App = Realm.React[/* SimpleApp */2](/* module */[
      /* init */init$1,
      /* view */view
    ]);

exports.Validation = Validation;
exports.isRequired = isRequired;
exports.isValidEmail = isValidEmail;
exports.isJson = isJson;
exports.isInteger = isInteger;
exports.validatedTextInput = validatedTextInput;
exports.App = App;
/* App Not a pure module */
