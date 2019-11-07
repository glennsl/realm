// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Vdom = require("../../src/Vdom.bs.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");

function log(patches) {
  console.log("--");
  return List.iter((function (prim) {
                console.log(prim);
                return /* () */0;
              }), List.map(Vdom.pp_patch, patches));
}

var domNode = document.getElementById("root");

var current = /* record */[/* contents */Vdom.$$Node[/* text */1]("")];

function render(state) {
  if (state) {
    return Vdom.$$Node[/* element */2](undefined, "div", /* :: */[
                /* Attribute */Block.__(0, [Vdom.Attribute[/* make */0](undefined, "class", "red")]),
                /* :: */[
                  /* Event */Block.__(1, [
                      "click",
                      (function (param) {
                          return onClick(state, param);
                        })
                    ]),
                  /* [] */0
                ]
              ], /* :: */[
                Vdom.$$Node[/* text */1]("Hello"),
                /* :: */[
                  Vdom.$$Node[/* element */2](undefined, "em", /* [] */0, /* :: */[
                        Vdom.$$Node[/* text */1](" world"),
                        /* [] */0
                      ]),
                  /* :: */[
                    Vdom.$$Node[/* element */2](undefined, "span", /* [] */0, /* :: */[
                          Vdom.$$Node[/* text */1]("!"),
                          /* [] */0
                        ]),
                    /* :: */[
                      Curry._4(Vdom.$$Node[/* Keyed */3][/* element */0], undefined, "ol", /* [] */0, /* :: */[
                            /* tuple */[
                              "1",
                              Vdom.$$Node[/* element */2](undefined, "li", /* [] */0, /* :: */[
                                    Vdom.$$Node[/* text */1]("1"),
                                    /* [] */0
                                  ])
                            ],
                            /* :: */[
                              /* tuple */[
                                "3",
                                Vdom.$$Node[/* element */2](undefined, "li", /* [] */0, /* :: */[
                                      Vdom.$$Node[/* text */1]("3"),
                                      /* [] */0
                                    ])
                              ],
                              /* :: */[
                                /* tuple */[
                                  "2",
                                  Vdom.$$Node[/* element */2](undefined, "li", /* [] */0, /* :: */[
                                        Vdom.$$Node[/* text */1]("2"),
                                        /* [] */0
                                      ])
                                ],
                                /* :: */[
                                  /* tuple */[
                                    "4",
                                    Vdom.$$Node[/* element */2](undefined, "li", /* [] */0, /* :: */[
                                          Vdom.$$Node[/* text */1]("4"),
                                          /* [] */0
                                        ])
                                  ],
                                  /* :: */[
                                    /* tuple */[
                                      "5",
                                      Vdom.$$Node[/* element */2](undefined, "li", /* [] */0, /* :: */[
                                            Vdom.$$Node[/* text */1]("5"),
                                            /* [] */0
                                          ])
                                    ],
                                    /* :: */[
                                      /* tuple */[
                                        "6",
                                        Vdom.$$Node[/* element */2](undefined, "li", /* [] */0, /* :: */[
                                              Vdom.$$Node[/* text */1]("6"),
                                              /* [] */0
                                            ])
                                      ],
                                      /* [] */0
                                    ]
                                  ]
                                ]
                              ]
                            ]
                          ]),
                      /* [] */0
                    ]
                  ]
                ]
              ]);
  } else {
    return Vdom.$$Node[/* element */2](undefined, "div", /* :: */[
                /* Attribute */Block.__(0, [Vdom.Attribute[/* make */0](undefined, "class", "blue")]),
                /* :: */[
                  /* Event */Block.__(1, [
                      "click",
                      (function (param) {
                          return onClick(state, param);
                        })
                    ]),
                  /* [] */0
                ]
              ], /* :: */[
                Vdom.$$Node[/* text */1]("Hi"),
                /* :: */[
                  Vdom.$$Node[/* element */2](undefined, "em", /* [] */0, /* :: */[
                        Vdom.$$Node[/* text */1](" world"),
                        /* [] */0
                      ]),
                  /* :: */[
                    Vdom.$$Node[/* element */2](undefined, "span", /* [] */0, /* :: */[
                          Vdom.$$Node[/* text */1]("!"),
                          /* [] */0
                        ]),
                    /* :: */[
                      Curry._4(Vdom.$$Node[/* Keyed */3][/* element */0], undefined, "ol", /* [] */0, /* :: */[
                            /* tuple */[
                              "1",
                              Vdom.$$Node[/* element */2](undefined, "li", /* [] */0, /* :: */[
                                    Vdom.$$Node[/* text */1]("1"),
                                    /* [] */0
                                  ])
                            ],
                            /* :: */[
                              /* tuple */[
                                "2",
                                Vdom.$$Node[/* element */2](undefined, "li", /* [] */0, /* :: */[
                                      Vdom.$$Node[/* text */1]("2"),
                                      /* [] */0
                                    ])
                              ],
                              /* :: */[
                                /* tuple */[
                                  "3",
                                  Vdom.$$Node[/* element */2](undefined, "li", /* [] */0, /* :: */[
                                        Vdom.$$Node[/* text */1]("3"),
                                        /* [] */0
                                      ])
                                ],
                                /* :: */[
                                  /* tuple */[
                                    "4",
                                    Vdom.$$Node[/* element */2](undefined, "li", /* [] */0, /* :: */[
                                          Vdom.$$Node[/* text */1]("4"),
                                          /* [] */0
                                        ])
                                  ],
                                  /* :: */[
                                    /* tuple */[
                                      "5",
                                      Vdom.$$Node[/* element */2](undefined, "li", /* [] */0, /* :: */[
                                            Vdom.$$Node[/* text */1]("5"),
                                            /* [] */0
                                          ])
                                    ],
                                    /* :: */[
                                      /* tuple */[
                                        "6",
                                        Vdom.$$Node[/* element */2](undefined, "li", /* [] */0, /* :: */[
                                              Vdom.$$Node[/* text */1]("6"),
                                              /* [] */0
                                            ])
                                      ],
                                      /* [] */0
                                    ]
                                  ]
                                ]
                              ]
                            ]
                          ]),
                      /* [] */0
                    ]
                  ]
                ]
              ]);
  }
}

function onClick(state, param) {
  var next = state ? render(/* First */0) : render(/* Second */1);
  var patches = Vdom.diff(domNode, current[0], next);
  log(patches);
  Vdom.patch(patches);
  current[0] = next;
  return /* () */0;
}

current[0] = render(/* First */0);

domNode.appendChild(Vdom.render(current[0]));

onClick(/* First */0, /* () */0);

exports.log = log;
exports.domNode = domNode;
exports.current = current;
exports.render = render;
exports.onClick = onClick;
/* domNode Not a pure module */
