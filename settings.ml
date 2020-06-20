type t = 
  {
    suit_colors : ANSITerminal.style list; 
    winning_score : int;
    stick_the_dealer : bool;
  }

open ANSITerminal

let default = {
  suit_colors = [ANSITerminal.green; ANSITerminal.blue; ANSITerminal.red; 
                 ANSITerminal.black]; 
  winning_score = 10;
  stick_the_dealer = true;
}

let set_suit_colors i s = 
  match i with
  | 1 -> {s with suit_colors = [ANSITerminal.green; ANSITerminal.blue; 
                                ANSITerminal.red; ANSITerminal.black]}
  | 2 -> {s with suit_colors = [ANSITerminal.black; ANSITerminal.red; 
                                ANSITerminal.red; ANSITerminal.black]}
  | _ -> s

let set_winning_score i s =
  if i <= 0 then s else
    {s with winning_score = i}

let set_stick_the_dealer b s = 
  {s with stick_the_dealer = b}

let get_suit_colors s = s.suit_colors

let get_winning_score s = s.winning_score

let get_stick_the_dealer s = s.stick_the_dealer

(** [color_to_string c] is the string corresponding to the name of the 
    ANSITerminal.Color [c]. *)
let color_to_string c = 
  match c with
  | Foreground Red -> "red"
  | Foreground Black -> "black"
  | Foreground Blue -> "blue"
  | Foreground Green -> "green"
  | _ -> failwith "Illegal Color"

let print_settings s = 
  print_string [] "Settings:\n";
  print_string [magenta]
    ("To change a field of the game settings, enter a command in the form of"
     ^ " \"set\" followed by the field name you wish to change "
     ^ "followed by the integer representing your desired option. To quit, use "
     ^ " the command \"quit\". To play the game"
     ^ "with the current settings, use the command \"play\".\n");
  Printf.printf 
    "\n\tColors : [Clubs = %s; Diamonds = %s; Hearts = %s; Spades = %s]" 
    (color_to_string (List.nth s.suit_colors 0)) 
    (color_to_string (List.nth s.suit_colors 1))
    (color_to_string (List.nth s.suit_colors 2))
    (color_to_string (List.nth s.suit_colors 3));
  print_string [] ("\n\t\t1 - [Clubs = green; Diamonds = blue; Hearts = red; "
                   ^ "Spades = black]"
                   ^ "\n\t\t2 - [Clubs = black; Diamonds = red; Hearts = red; "
                   ^ "Spades = black]\n");
  Printf.printf "\n\tWinning Score : %i" s.winning_score;
  print_string [] "\n\t\ti - winning score is set to integer value i\n";
  Printf.printf "\n\tStick the Dealer: %b" s.stick_the_dealer;
  print_string [] "\n\t\t1 - false\n\t\t2 - true\n"

