// Generated by BUCKLESCRIPT VERSION 4.0.8, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var $$Array = require("bs-platform/lib/js/array.js");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");

function differsByOne(l, r) {
  var left = l.split("");
  var right = r.split("");
  var numLettersInCommon = Belt_Array.reduceWithIndex(left, 0, (function (acc, _x, idx) {
          if (Caml_array.caml_array_get(left, idx) === Caml_array.caml_array_get(right, idx)) {
            return acc + 1 | 0;
          } else {
            return acc;
          }
        }));
  var match = left.length - numLettersInCommon | 0;
  return match === 1;
}

function findCorrectIds(x, l) {
  var result = Belt_List.keep(l, (function (a) {
          if (a.length > 0) {
            return differsByOne(x, a);
          } else {
            return false;
          }
        }));
  var match = Belt_List.size(result);
  if (match !== 1) {
    return /* tuple */[
            undefined,
            undefined
          ];
  } else {
    return /* tuple */[
            x,
            Belt_List.head(result)
          ];
  }
}

function doFindCorrectIds(acc, _l) {
  while(true) {
    var l = _l;
    if (l) {
      var t = l[1];
      if (Belt_List.length(t) > 0) {
        var match = findCorrectIds(l[0], t);
        var match$1 = match[0];
        if (match$1 !== undefined) {
          var match$2 = match[1];
          if (match$2 !== undefined) {
            return /* tuple */[
                    match$1,
                    match$2
                  ];
          } else {
            _l = t;
            continue ;
          }
        } else {
          _l = t;
          continue ;
        }
      } else {
        return acc;
      }
    } else {
      return acc;
    }
  };
}

var result = doFindCorrectIds(/* tuple */[
      "",
      ""
    ], $$Array.to_list(Fs.readFileSync("./src/day-2-input.txt", "utf8").split("\n")));

console.log(result);

exports.differsByOne = differsByOne;
exports.findCorrectIds = findCorrectIds;
exports.doFindCorrectIds = doFindCorrectIds;
exports.result = result;
/* result Not a pure module */
