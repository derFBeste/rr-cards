open Js.Option;

type suit =
  | Hearts
  | Diamonds
  | Clubs
  | Spades;

type value =
  | Ace
  | King
  | Queen
  | Jack
  | Num(int); // constrain to 2-10

type card =
  | Card(value, suit)
  | Joker;

module RenderToString = {
  let suitToString = suit =>
    switch (suit) {
    | Hearts => "Hearts"
    | Diamonds => "Diamonds"
    | Clubs => "Clubs"
    | Spades => "Spades"
    };

  let numToString = num =>
    switch (num) {
    | 2 => "Two"
    | 3 => "Three"
    | 4 => "Four"
    | 5 => "Five"
    | 6 => "Six"
    | 7 => "Seven"
    | 8 => "Eight"
    | 9 => "Nine"
    | 10 => "Ten"
    | _ => failwith("this is an exception from numToString")
    };

  let valueToString = value =>
    switch (value) {
    | Ace => "Ace"
    | King => "King"
    | Queen => "Queen"
    | Jack => "Jack"
    | Num(n) => numToString(n)
    };

  let renderCard = card =>
    switch (card) {
    | Card(value, suit) =>
      valueToString(value) ++ " of " ++ suitToString(suit)
    | Joker => "Joker"
    };

  let defaulErrorCard = "-- unknown card --";
};

// Card(Num(8), Hearts) |> renderCard |> Js.log;
module Parser = {
  let parseNumValue = numStr => {
    let parsed =
      try(numStr |> int_of_string |> some) {
      | Failure(_) => None
      };

    switch (parsed) {
    | Some(n) when n >= 2 && n <= 10 => Some(Num(n))
    | _ => None
    };
  };

  let parseSuit = suitStr =>
    switch (suitStr) {
    | "H" => Some(Hearts)
    | "D" => Some(Diamonds)
    | "C" => Some(Clubs)
    | "S" => Some(Spades)
    | _ => None
    };

  let parseValue = valueStr =>
    switch (valueStr) {
    | "A" => Some(Ace)
    | "K" => Some(King)
    | "Q" => Some(Queen)
    | "J" => Some(Jack)
    | n => parseNumValue(n)
    };

  let parseCard = cardStr => {
    let length = Js.String.length(cardStr);
    let suitStr = Js.String.sliceToEnd(~from=length - 1, cardStr);
    let valueStr = Js.String.slice(~from=0, ~to_=length - 1, cardStr);
    switch (parseValue(valueStr), parseSuit(suitStr)) {
    | (Some(value), Some(suit)) => Card(value, suit) |> some
    | _ => None
    };
  };

  let parseWithJoker = cardStr =>
    switch (cardStr) {
    | "J" => Some(Joker)
    | str => parseCard(str)
    };
};

module Option = {
  let map = (fn, opt) =>
    switch (opt) {
    | Some(x) => fn(x) |> some
    | None => None
    };

  // why is this necessary?
  let default = (defaultValue, opt) =>
    switch (opt) {
    | Some(x) => x
    | None => defaultValue
    };
};

"AS"
|> Parser.parseCard
|> Option.map(RenderToString.renderCard)
|> Option.default(RenderToString.defaulErrorCard)
|> Js.log;

"J"
|> Parser.parseWithJoker
|> Option.map(RenderToString.renderCard)
|> Option.default(RenderToString.defaulErrorCard)
|> Js.log;

// "AX"
// |> parseCard
// |> optionMap(renderCard)
// |> optionWithDefault("-- unknown card --")
// |> Js.log;
// "XA" |> parseCard |> optionMap(renderCard) |> Js.log;
