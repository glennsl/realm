// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");

function make(namespace, key, value) {
  return /* record */[
          /* namespace */namespace,
          /* key */key,
          /* value */value
        ];
}

var Attribute = /* module */[/* make */make];

function dekey(node) {
  switch (node.tag | 0) {
    case 0 : 
    case 1 : 
        return node;
    case 2 : 
        var match = node[0];
        return /* Element */Block.__(1, [/* record */[
                    /* namespace */match[/* namespace */0],
                    /* tag */match[/* tag */1],
                    /* properties */match[/* properties */2],
                    /* children */List.map((function (param) {
                            return dekey(param[1]);
                          }), match[/* children */3])
                  ]]);
    
  }
}

function text(s) {
  return /* Text */Block.__(0, [s]);
}

function element(namespace, tag, properties, children) {
  return /* Element */Block.__(1, [/* record */[
              /* namespace */namespace,
              /* tag */tag,
              /* properties */properties,
              /* children */children
            ]]);
}

var $$Node = /* module */[
  /* dekey */dekey,
  /* text */text,
  /* element */element
];

var Dom = /* module */[];

function append(targetNode, node) {
  var domNode;
  switch (node.tag | 0) {
    case 0 : 
        domNode = document.createTextNode(node[0]);
        break;
    case 1 : 
        var spec = node[0];
        var match = spec[/* namespace */0];
        var el = match !== undefined ? document.createElementNS(match, spec[/* tag */1]) : document.createElement(spec[/* tag */1]);
        List.iter((function (param) {
                var attr = param[0];
                var match = attr[/* namespace */0];
                if (match !== undefined) {
                  el.setAttribute(match, attr[/* key */1], attr[/* value */2]);
                  return /* () */0;
                } else {
                  el.setAttribute(attr[/* key */1], attr[/* value */2]);
                  return /* () */0;
                }
              }), spec[/* properties */2]);
        List.iter((function (param) {
                return append(el, param);
              }), spec[/* children */3]);
        domNode = el;
        break;
    case 2 : 
        domNode = Pervasives.failwith("todo");
        break;
    
  }
  targetNode.appendChild(domNode);
  return /* () */0;
}

function render(node, targetId) {
  var domNode = document.getElementById(targetId);
  append(domNode, node);
  return domNode;
}

function diff(rootDomNode, oldVTree, newVTree) {
  var diffNode = function (domNode, patches, oVNode, _nVNode) {
    while(true) {
      var nVNode = _nVNode;
      var exit = 0;
      switch (oVNode.tag | 0) {
        case 0 : 
            switch (nVNode.tag | 0) {
              case 0 : 
                  var nText = nVNode[0];
                  if (oVNode[0] === nText) {
                    return patches;
                  } else {
                    return /* :: */[
                            /* SetText */Block.__(3, [
                                domNode,
                                nText
                              ]),
                            patches
                          ];
                  }
              case 1 : 
              case 2 : 
                  exit = 1;
                  break;
              
            }
            break;
        case 1 : 
            var o = oVNode[0];
            switch (nVNode.tag | 0) {
              case 0 : 
                  exit = 1;
                  break;
              case 1 : 
                  var n = nVNode[0];
                  if (o[/* tag */1] === n[/* tag */1] && Caml_obj.caml_equal(o[/* namespace */0], n[/* namespace */0])) {
                    var patches$1 = diffProperties(domNode, patches, o[/* properties */2], n[/* properties */2]);
                    var parentDomNode = domNode;
                    var _patches = patches$1;
                    var _oldVNodes = o[/* children */3];
                    var _newVNodes = n[/* children */3];
                    var _index = 0;
                    while(true) {
                      var index = _index;
                      var newVNodes = _newVNodes;
                      var oldVNodes = _oldVNodes;
                      var patches$2 = _patches;
                      if (oldVNodes) {
                        if (newVNodes) {
                          var childDomNodes = parentDomNode.childNodes;
                          var probablyDomNode = childDomNodes[index];
                          var patches$3 = probablyDomNode !== undefined ? diffNode(probablyDomNode, patches$2, oldVNodes[0], newVNodes[0]) : Pervasives.failwith("well this shouldn't happen");
                          _index = index + 1 | 0;
                          _newVNodes = newVNodes[1];
                          _oldVNodes = oldVNodes[1];
                          _patches = patches$3;
                          continue ;
                        } else {
                          return /* :: */[
                                  /* PopNodes */Block.__(2, [
                                      parentDomNode,
                                      List.length(oldVNodes)
                                    ]),
                                  patches$2
                                ];
                        }
                      } else if (newVNodes) {
                        return /* :: */[
                                /* PushNodes */Block.__(1, [
                                    parentDomNode,
                                    newVNodes
                                  ]),
                                patches$2
                              ];
                      } else {
                        return patches$2;
                      }
                    };
                  } else {
                    return /* :: */[
                            /* Rerender */Block.__(0, [
                                domNode,
                                nVNode
                              ]),
                            patches
                          ];
                  }
              case 2 : 
                  _nVNode = dekey(nVNode);
                  continue ;
              
            }
            break;
        case 2 : 
            var o$1 = oVNode[0];
            switch (nVNode.tag | 0) {
              case 0 : 
              case 1 : 
                  exit = 1;
                  break;
              case 2 : 
                  var n$1 = nVNode[0];
                  if (o$1[/* tag */1] === n$1[/* tag */1] && Caml_obj.caml_equal(o$1[/* namespace */0], n$1[/* namespace */0])) {
                    return patches;
                  } else {
                    return /* :: */[
                            /* Rerender */Block.__(0, [
                                domNode,
                                nVNode
                              ]),
                            patches
                          ];
                  }
              
            }
            break;
        
      }
      if (exit === 1) {
        return /* :: */[
                /* Rerender */Block.__(0, [
                    domNode,
                    nVNode
                  ]),
                patches
              ];
      }
      
    };
  };
  var diffProperties = function (domNode, patches, oldProperties, newProperties) {
    var isMatch = function (x, y) {
      var n = y[0];
      var o = x[0];
      if (Caml_obj.caml_equal(o[/* namespace */0], n[/* namespace */0])) {
        return o[/* key */1] === n[/* key */1];
      } else {
        return false;
      }
    };
    var onResult = function (patches, x, y) {
      if (x !== undefined) {
        var oldProperty = x;
        if (y !== undefined) {
          var newProperty = y;
          if (oldProperty[0][/* value */2] !== newProperty[0][/* value */2]) {
            return /* :: */[
                    /* SetProperty */Block.__(4, [
                        domNode,
                        newProperty
                      ]),
                    patches
                  ];
          } else {
            return patches;
          }
        } else {
          return /* :: */[
                  /* RemoveProperty */Block.__(5, [
                      domNode,
                      oldProperty
                    ]),
                  patches
                ];
        }
      } else if (y !== undefined) {
        return /* :: */[
                /* SetProperty */Block.__(4, [
                    domNode,
                    y
                  ]),
                patches
              ];
      } else {
        return patches;
      }
    };
    var isMatch$1 = isMatch;
    var onResult$1 = onResult;
    var acc = patches;
    var allXs = oldProperties;
    var allYs = newProperties;
    var processXs = function (_acc, _xs, _ys) {
      while(true) {
        var ys = _ys;
        var xs = _xs;
        var acc = _acc;
        if (xs) {
          var remainingXs = xs[1];
          var x = xs[0];
          if (ys) {
            var y = ys[0];
            if (Curry._2(isMatch$1, x, y)) {
              var acc$1 = Curry._3(onResult$1, acc, Caml_option.some(x), Caml_option.some(y));
              _ys = allYs;
              _xs = remainingXs;
              _acc = acc$1;
              continue ;
            } else {
              _ys = ys[1];
              continue ;
            }
          } else {
            var acc$2 = Curry._3(onResult$1, acc, Caml_option.some(x), undefined);
            _ys = allYs;
            _xs = remainingXs;
            _acc = acc$2;
            continue ;
          }
        } else {
          return acc;
        }
      };
    };
    var acc$1 = processXs(acc, allXs, allYs);
    var _acc = acc$1;
    var _xs = allXs;
    var _ys = allYs;
    while(true) {
      var ys = _ys;
      var xs = _xs;
      var acc$2 = _acc;
      if (ys) {
        if (xs) {
          if (Curry._2(isMatch$1, xs[0], ys[0])) {
            _ys = ys[1];
            continue ;
          } else {
            _xs = xs[1];
            continue ;
          }
        } else {
          var acc$3 = Curry._3(onResult$1, acc$2, undefined, Caml_option.some(ys[0]));
          _ys = ys[1];
          _xs = allXs;
          _acc = acc$3;
          continue ;
        }
      } else {
        return acc$2;
      }
    };
  };
  var match = rootDomNode.firstChild;
  if (match !== undefined) {
    return diffNode(match, /* [] */0, oldVTree, newVTree);
  } else {
    return Pervasives.failwith("no dom");
  }
}

function pp_node(param) {
  switch (param.tag | 0) {
    case 0 : 
        return "Text " + (String(param[0]) + " ");
    case 1 : 
        return "Element " + (String(param[0][/* tag */1]) + " ");
    case 2 : 
        return "KeyedElement " + (String(param[0][/* tag */1]) + " ");
    
  }
}

function pp_patch(param) {
  switch (param.tag | 0) {
    case 0 : 
        var text = pp_node(param[1]);
        return "Rerender " + (String(text) + "");
    case 1 : 
        var length = List.length(param[1]);
        return "PushNodes " + (String(length) + "");
    case 2 : 
        return "PopNodes " + (String(param[1]) + " ");
    case 3 : 
        return "SetText " + (String(param[1]) + "");
    case 4 : 
        return "SetProperty " + (String(param[1]) + "");
    case 5 : 
        return "RemoveProperty " + (String(param[1]) + "");
    
  }
}

exports.Attribute = Attribute;
exports.$$Node = $$Node;
exports.Dom = Dom;
exports.append = append;
exports.render = render;
exports.diff = diff;
exports.pp_node = pp_node;
exports.pp_patch = pp_patch;
/* No side effect */
