type suit = Deck.suit
type card = Deck.card

type is_alone = State.is_alone
type t = 
  | Pass
  | Pick of is_alone
  | Drop of card
  | Declare of suit * is_alone
  | Play of card
  | Quit

exception IllegalCommand

(** [parse_helper lst] is the command corresponding to the the strings from 
    [lst].
    Raises IllegalCommand if [lst] does not respresent a valid command*)
let parse_helper lst = 
  match lst with
  | "pass" :: [] -> Pass
  | "drop" :: c :: "of" :: s :: [] -> 
    Drop (Deck.val_from_str c, Deck.suit_from_str s)
  | "pick" :: "it" :: "up" :: "alone" :: [] | "pick" :: "alone" :: [] -> 
    Pick true
  | "pick" :: "it" :: "up" :: [] | "pick" :: [] -> 
    Pick false
  | "declare" :: s :: "alone" :: [] -> Declare ((Deck.suit_from_str s), true)
  | "declare" :: s :: _ -> Declare ((Deck.suit_from_str s), false)
  | "play" :: c :: "of" :: s :: [] | c :: "of" :: s :: [] -> 
    Play (Deck.val_from_str c, Deck.suit_from_str s)
  | "quit" :: [] -> Quit
  | _ -> raise IllegalCommand

let parse str = 
  str |> String.trim |> String.lowercase_ascii |> String.split_on_char ' ' 
  |> List.filter (fun x -> (x <> "")) |> parse_helper