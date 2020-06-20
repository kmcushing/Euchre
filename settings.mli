(** Represents settings for the game. *)

(* The type [t] represents a data structure for the settings for a game of 
   euchre. *)
type t

(** [default] represents the default settings for the game. *)
val default : t

(** [set_suit_colors i s] is the representation of game settings achieved by 
    updating the suit colors of [s] to be that of option [i] displayed in the 
    text-based interface. Returns [s] if [i] is not a valid input. *)
val set_suit_colors : int -> t -> t

(** [set_winning_score i s] is the representation of game settings achieved by 
    updating the winning score of [s] to be [i]. Returns [s] if [i] is 
    negative. *)
val set_winning_score : int -> t -> t

(** [set_canadian_loner b t] is the representation of game settings achieved by
    updating the stick the dealer condition of [s] to be [b]. *)
val set_stick_the_dealer : bool -> t -> t

(** [get_suit_colors s] is the list of colors corresponding to suits in [s]. *)
val get_suit_colors : t -> ANSITerminal.style list

(** [get_winning_score s] is the score needed to win a game with settings [s].
*)
val get_winning_score : t -> int

(** [get_stick_the_dealer s] is whether or not the settings represented by [s] 
    require the dealer to declare a suit after being passed around to twice. *)
val get_stick_the_dealer : t -> bool

(** [print_settings s] prints out the settings [s] in a text format.*)
val print_settings : t -> unit