type value = 
  | Nine 
  | Ten
  | Jack
  | Queen
  | King
  | Ace

type suit = 
  | Clubs
  | Diamonds
  | Hearts
  | Spades

type card = (value * suit)
type t = card list

exception InvalidSuit
exception InvalidCardValue

let suits = [Clubs; Diamonds; Hearts; Spades]
let values = [Nine; Ten; Jack; Queen; King; Ace]


(** [deck_loop 0 0 []] is a new euchre deck. *)
let rec deck_loop i j lst = 
  if List.length lst = 24 then 
    lst 
  else 
  if (((j + 1) mod 6) = 0) && (j <> 0) then
    deck_loop (i + 1) 0 ((List.nth values j, List.nth suits i)::lst)
  else
    deck_loop i (j + 1) ((List.nth values j, List.nth suits i)::lst)


let init = 
  deck_loop 0 0 []

let deal_card d = 
  Random.self_init ();
  let temp = List.nth d (Random.int (List.length d)) in
  (temp, (List.filter (fun x -> x <> temp) d))

let suit_from_str s = 
  match s with
    "clubs" | "c" -> Clubs
  | "diamonds" | "d" -> Diamonds
  | "hearts" | "h" -> Hearts
  | "spades" | "s" -> Spades
  | _ -> raise InvalidSuit

let val_from_str s = 
  match s with
  | "9" | "nine" -> Nine
  | "10" | "ten" | "t" -> Ten
  | "j" | "jack" -> Jack
  | "q" | "queen" -> Queen
  | "k" | "king" -> King
  | "a" | "ace" -> Ace
  | _ -> raise InvalidCardValue

(** [aux_deal_hand d lst] is a tuple (h,r) where h is the card list of the hand 
    of length five dealt from deck [d] with the cards from [lst] already dealt, 
    and r is the remaining deck*)
let rec aux_deal_hand d lst = 
  if List.length lst = 5 then 
    (lst, d)
  else
    let (c, d2) = deal_card d in
    aux_deal_hand d2 (c::lst)

let deal_hand d =
  aux_deal_hand d []

let suit_to_string (s : suit) = 
  match s with 
  | Clubs -> "CLUBS"
  | Diamonds -> "DIAMONDS"
  | Hearts -> "HEARTS"
  | Spades -> "SPADES"

let val_to_string (v : value) = 
  match v with
  | Nine -> "9"
  | Ten -> "T"
  | Jack -> "J"
  | Queen -> "Q"
  | King -> "K"
  | Ace -> "A"

let card_to_string c =
  match c with
  | (v, s) -> (val_to_string v) ^ " " ^ (suit_to_string s)

let card_to_strings_and_styles c settings = 
  match Settings.get_suit_colors settings with
  | clubs :: diamonds :: hearts :: spades :: [] ->
    let (v,s) = c in
    let (style, art_1, art_2, art_3, art_4) = 
      (match s with
       | Clubs -> ([clubs; ANSITerminal.on_white], "     _     ", 
                   "   _(_)_   ", "  (_/*\\_)  ", "     |     ")
       | Spades -> ([spades; ANSITerminal.on_white], "     .     ",
                    "    / \\    ", "   (/|\\)   ", "     |     ")
       | Diamonds -> ([diamonds; ANSITerminal.on_white], "    /\\     ",
                      "   /  \\    ", "   \\  /    ", "    \\/     ")
       | Hearts -> ([hearts; ANSITerminal.on_white], "   _  _    ",
                    "  ( \\/ )   ", "   \\  /    ", "    \\/     ")) in
    let strings = [
      val_to_string v ^ "          ";
      art_1; art_2; art_3; art_4;
      "          " ^ val_to_string v;
    ] in
    (strings, style)
  | _ -> failwith "Illegal Settings"

