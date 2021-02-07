// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Js_option = require("bs-platform/lib/js/js_option.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");
var Caml_js_exceptions = require("bs-platform/lib/js/caml_js_exceptions.js");

function suitToString(suit) {
  switch (suit) {
    case /* Hearts */0 :
        return "Hearts";
    case /* Diamonds */1 :
        return "Diamonds";
    case /* Clubs */2 :
        return "Clubs";
    case /* Spades */3 :
        return "Spades";
    
  }
}

function numToString(num) {
  switch (num) {
    case 2 :
        return "Two";
    case 3 :
        return "Three";
    case 4 :
        return "Four";
    case 5 :
        return "Five";
    case 6 :
        return "Six";
    case 7 :
        return "Seven";
    case 8 :
        return "Eight";
    case 9 :
        return "Nine";
    case 10 :
        return "Ten";
    default:
      return Pervasives.failwith("this is an exception from numToString");
  }
}

function valueToString(value) {
  if (typeof value !== "number") {
    return numToString(value._0);
  }
  switch (value) {
    case /* Ace */0 :
        return "Ace";
    case /* King */1 :
        return "King";
    case /* Queen */2 :
        return "Queen";
    case /* Jack */3 :
        return "Jack";
    
  }
}

function renderCard(card) {
  if (card) {
    return valueToString(card._0) + (" of " + suitToString(card._1));
  } else {
    return "Joker";
  }
}

var defaulErrorCard = "-- unknown card --";

var RenderToString = {
  suitToString: suitToString,
  numToString: numToString,
  valueToString: valueToString,
  renderCard: renderCard,
  defaulErrorCard: defaulErrorCard
};

function parseNumValue(numStr) {
  var parsed;
  try {
    parsed = Js_option.some(Caml_format.caml_int_of_string(numStr));
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Failure") {
      parsed = undefined;
    } else {
      throw exn;
    }
  }
  if (parsed !== undefined && parsed >= 2 && parsed <= 10) {
    return /* Num */{
            _0: parsed
          };
  }
  
}

function parseSuit(suitStr) {
  switch (suitStr) {
    case "C" :
        return /* Clubs */2;
    case "D" :
        return /* Diamonds */1;
    case "H" :
        return /* Hearts */0;
    case "S" :
        return /* Spades */3;
    default:
      return ;
  }
}

function parseValue(valueStr) {
  switch (valueStr) {
    case "A" :
        return /* Ace */0;
    case "J" :
        return /* Jack */3;
    case "K" :
        return /* King */1;
    case "Q" :
        return /* Queen */2;
    default:
      return parseNumValue(valueStr);
  }
}

function parseCard(cardStr) {
  var length = cardStr.length;
  var suitStr = cardStr.slice(length - 1 | 0);
  var valueStr = cardStr.slice(0, length - 1 | 0);
  var match = parseValue(valueStr);
  var match$1 = parseSuit(suitStr);
  if (match !== undefined && match$1 !== undefined) {
    return Js_option.some(/* Card */{
                _0: match,
                _1: match$1
              });
  }
  
}

function parseWithJoker(cardStr) {
  if (cardStr === "J") {
    return /* Joker */0;
  } else {
    return parseCard(cardStr);
  }
}

var Parser = {
  parseNumValue: parseNumValue,
  parseSuit: parseSuit,
  parseValue: parseValue,
  parseCard: parseCard,
  parseWithJoker: parseWithJoker
};

function map(fn, opt) {
  if (opt !== undefined) {
    return Js_option.some(Curry._1(fn, Caml_option.valFromOption(opt)));
  }
  
}

function $$default(defaultValue, opt) {
  if (opt !== undefined) {
    return Caml_option.valFromOption(opt);
  } else {
    return defaultValue;
  }
}

var $$Option = {
  map: map,
  $$default: $$default
};

console.log($$default(defaulErrorCard, map(renderCard, parseCard("AS"))));

console.log($$default(defaulErrorCard, map(renderCard, parseWithJoker("J"))));

exports.RenderToString = RenderToString;
exports.Parser = Parser;
exports.$$Option = $$Option;
/*  Not a pure module */