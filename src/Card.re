let cardSuitLabel = suitStr =>
  switch (suitStr) {
  | "C" => Some("Clubs")
  | "D" => Some("Diamonds")
  | "H" => Some("Hearts")
  | "S" => Some("Spades")
  | _ => None
  };

let cardValueLabel = valueStr =>
  switch (valueStr) {
  | "2" => Some("Two")
  | "3" => Some("Three")
  | "4" => Some("Four")
  | "5" => Some("Five")
  | "6" => Some("Six")
  | "7" => Some("Seven")
  | "8" => Some("Eight")
  | "9" => Some("Nine")
  | "10" => Some("Ten")
  | "J" => Some("Jack")
  | "Q" => Some("Queen")
  | "K" => Some("King")
  | "A" => Some("Ace")
  | _ => None
  };

// @TODO: return suit icon
let cardSuitIcon = suitStr =>
  switch (suitStr) {
  | "C" => "Clubs"
  | "D" => "Diamonds"
  | "H" => "Hearts"
  | "S" => "Spades"
  | _ => "-- unknown suit --"
  };
