(** A representation of a state in a game of euchre.
    Each state keeps track of *)

(* The type [t] represents a game state. *)
type t

(* The type [card] represents a playing card. *)
type card = Deck.card

(* The type [suit] represents the suit of a playing card. *)
type suit = Deck.suit

(* The type [value] represents the value of a card in a euchre game. *)
type value = Deck.value

(* The type [is_alone] represents whether or not a declaring player is going 
    alone. *)
type is_alone = bool

(* Raised when an attempt is made to modify a state in an  illegal way. *)
exception IllegalState

(** [settings s] is the settings of the game in state [s]. *)
val settings : t -> Settings.t

(** [suit_of_card c t] is the suit that card [c] belongs to for the purposes
    of identifying reneges in a state where the trump suit is [t]. *)
val suit_of_card : card -> suit option -> suit

(** [compare_cards c1 c2 t l] is >0 if [c1] is better than [c2] in the state 
    with trump [t] and suit_lead [s], =0 is [c1] and [c2] are the same, and is 
    <0 otherwise. *)
val compare_cards : card -> card -> suit -> suit -> int

(** [game_over s] returns None if the game is not over and returns Some x 
    where x is the winning team if the game is over. *)
val game_over : t -> int option

(** [init settings] is the game state at the beginning of a game with settings
    [settings]. *)
val init : Settings.t -> t

(** [play c s] is the state s ting from the current player playing card c in 
    state [s].
    Raises  [IllegalState] if card [c] is not in the current player's hand or 
    if playing card [c] is illegal in state [s]. *)
val play : card -> t -> t

(** [pick b s] is the state resulting from the current player telling the dealer 
    to pick up the up card in state [s]. [b] represents whether or not the 
    declaring player is choosing to go alone. 
    Raises [IllegalState] if there is no up card in state [s] or if it is 
    illegal to declare a suit in state [s]. *)
val pick : is_alone -> t -> t

(** [drop c s] is the state resulting from the current player attempting to 
    drop card [c] in state [s]. 
    Raises [IllegalState] if dropping a card is not a legal move in state 
    [s]. *)
val drop : card -> t -> t

(** [pass s] is the state resulting from an attempt to pass in [s]. 
    Raises [IllegalState] if passing is not a legal move in state [s]. *)
val pass : t -> t

(** [declare suit s] is the state resulting from attempting to declare [suit] 
    as the trump suit in state [s]. 
    Raises [IllegalState] if declaring a suit is not a legal move in state 
    [s]. *)
val declare : suit -> is_alone -> t -> t

(** [print_hand s] prints out the current player's hand in state [s]. *)
val print_hand : t -> unit

(** [print_without_hand s] prints a representation of state [s] without showing 
    the current player's hand. *)
val print_without_hand : t -> unit