(** Represents a command from a player. *)

(* The type [suit] represents the suit of a card. *)
type suit = Deck.suit

(* The type [card] represents a card. *)
type card = Deck.card
type is_alone = State.is_alone
(* The type [t] represents a command from a player. *)
type t = 
  | Pass
  | Pick of is_alone
  | Drop of card
  | Declare of suit * is_alone
  | Play of card
  | Quit

(** Raised when an illegal command is parsed. *)
exception IllegalCommand

(** [parse s] is the command resulting from parsing the string [s].
    Valid values for [s] take one of the following forms:
    - "pick it up"
    - "pick it up alone"
    - "pass"
    - "declare s" where s is the name of a suit
    - "declare s alone" where s is the name of a suit
    - "play c" where c is a card in the form (value ^ " of " ^ suit)
    - "quit"
    - "drop c" where c is a card in the form (value ^ " of " ^ suit)

    Note: parsing should ignore case and extra spaces between words
    Raises: [IllegalCommand] if [s] is an invalid value. *)
val parse : string -> t