(** Represents a euchre deck. *)

(* The type [value] represents the face value of a card in a euchre deck. 
*)
type value = 
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace

(* The type [suit] represents the suit of a card in a euchre deck. *)
type suit = 
  | Clubs
  | Diamonds
  | Hearts
  | Spades

(* The type [card] represents a card in a euchre deck. *)
type card = (value * suit)

(* The type t represents a deck of cards. *)
type t = card list

(** Raised when an invalid suit is passed in. *)
exception InvalidSuit

(** Raised when an invalid card value is passed in. *)
exception InvalidCardValue

(** [init] is a euchre deck. *)
val init : t

(** [deal_card d] is a tuple where the first value is a random card in [d] and 
    the second value is the deck [d] with the dealt card removed. *)
val deal_card : t -> (card * t)

(** [suit_from_str s] is the suit named in [s].
    Raises: [InvalidSuit] if [s] is not the name of a suit. 

    Precondition: [s] is a string of lowercase letters. *)
val suit_from_str : string -> suit

(** [val_from_str s] is the card value named in [s].
    Raises: [InvalidCardValue] if [s] is not the name of a card value in a 
    euchre deck. 

    Precondition: [s] is a string of lowercase letters. *)
val val_from_str : string -> value

(** [deal_hand d] is a tuple (h,r) where h is the card list of length five 
    representing the hand dealt from deck [d] and d is the remaining deck*)
val deal_hand : t -> (card list * t)

(** [card_to_string c] is the string representation of card [c]*)
val card_to_string : card -> string

(** [suit-to_string s] is the string representation of suit [s]*)
val suit_to_string : suit -> string

(** [card_to_strings_and_styles c s] is a tuple (strings,style) where strings is
    the string representation of the card [c] and style is an 
    ANSITerminal.style list that represents card styles according to the 
    settings [s]. *)
val card_to_strings_and_styles : card -> Settings.t 
  -> (string list * ANSITerminal.style list)