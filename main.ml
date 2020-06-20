type t = unit

(** [pass_message] is the string of the message following a pass*)
let pass_message = "Please pass the computer to the next player and then press " 
                   ^ "'Enter' to view next player's hand."

(** [pick_it_message] is the string of the message following a pick command. *)
let pick_it_message = "The dealer has been told to pick up the upcard. Press " 
                      ^ "'Enter' to view next player's hand then use a 'drop" ^
                      " [card]' command to discard that card from your hand\n"

(** [turn s message] erases the terminal and then prints the string [message] 
    before prompting the user to enter a new command before returning () *)
let rec turn (s : State.t) (message : string) : unit =
  ANSITerminal.erase Above;
  match State.game_over s with
  | Some x -> win x s
  | None ->
    ((* allows us to pause until a user presses enter *)
      State.print_without_hand s;
      ANSITerminal.(print_string [magenta] message);
      (read_line () |> (fun x -> ()));
      State.print_hand s;
      print_string "> ";
      let input = read_line () in
      try
        match  input |> Command.parse with
        | Pass -> turn (s |> State.pass) pass_message
        | Pick (alone) -> turn (s |> State.pick alone) pick_it_message
        | Drop card -> 
          turn (s |> State.drop card) (pass_message)
        | Declare (suit, alone) ->  turn (s |> State.declare suit alone) 
                                      pass_message
        | Play card -> turn (s |> State.play card) pass_message
        | Quit -> exit 0
      with
      | Command.IllegalCommand -> 
        turn s "You entered an illegal command press 'Enter' to try again."
      | Deck.InvalidSuit -> 
        turn s "The suit you entered is invalid. Press 'Enter' to try again."
      | Deck.InvalidCardValue -> 
        turn s "The card you entered is invalid. Press 'Enter' to try again."
      | State.IllegalState -> 
        turn s "You are not allowed to do that. Press 'Enter' to try again.")

(** [win i] prints the win message for team with id [i] and prompts the user to 
    quit or begin a new game.*)
and win i s = 
  let team_str = match i with
    | 1 -> "Green"
    | 2 -> "Red" 
    | _ -> failwith "not a valid team" in
  print_string (Printf.sprintf ("%s Team won the game!\nTo play again, press 'Enter'. To quit, type \"quit\" and then press enter.\n")
                  team_str);
  let input = read_line () in
  if String.trim input |> String.lowercase_ascii = "quit" then exit 0  else
    turn (State.init (State.settings s)) pass_message

(* The type [setting_command] represents a command to modify the game settings.
*)
type setting_command = Colors of int 
                     | WinningScore of int 
                     | StickTheDealer of bool
                     | Play

(* Raised when an illegal settings command is entered. *)
exception IllegalSettingsCommand

(** [settings_command_from_str_list l] is the [setting_command] corresponding
    to the strings in string list [l]. 
    Raises [IllegalSettingsCommand] if [l] is an illegal settings command. *)
let setting_command_from_str_list lst = 
  match lst with 
  | "set" :: "colors" :: i :: [] -> 
    (try let i' = int_of_string i in 
       if i' = 1 || i' = 2 then Colors i' else raise IllegalSettingsCommand
     with 
     | Failure _ -> raise IllegalSettingsCommand)
  | "set" :: "winning" :: "score" :: i :: [] -> 
    (try let i' = int_of_string i in
       if i' > 0 then WinningScore i' else raise IllegalSettingsCommand
     with
     | Failure _ -> raise IllegalSettingsCommand)
  | "set" :: "stick" :: "the" :: "dealer" :: "2" :: [] -> StickTheDealer true
  | "set" :: "stick" :: "the" :: "dealer" :: "1" :: [] -> 
    StickTheDealer false
  | "play" :: [] -> Play
  | "quit" :: [] -> exit 0
  | _ -> raise IllegalSettingsCommand

(** [settings s] updates the settings [s] according to user input. *)
let rec settings (s : Settings.t) =
  ANSITerminal.erase Above;
  Settings.print_settings s;
  print_string "> ";
  let input = read_line () |> String.trim |> String.lowercase_ascii 
              |> String.split_on_char ' ' |> List.filter (fun x -> (x <> "")) in
  try
    match setting_command_from_str_list input with
    | Colors i -> settings (Settings.set_suit_colors i s)
    | Play -> s
    | WinningScore i -> settings (Settings.set_winning_score i s)
    | StickTheDealer b -> settings (Settings.set_stick_the_dealer b s)
  with
  | IllegalSettingsCommand -> 
    ANSITerminal.(print_string [magenta] 
                    ("That is an illegal settings command press 'Enter' to try" 
                     ^ " again."));
    read_line () |> (fun x -> ());
    settings s

(** [rules ()] updates Settings.default using user input if the user chooses to
    change the settings. If not, Settings.default is returned. *)
let rules () = 
  ANSITerminal.erase Above;
  (print_string 
     ("The deck:\n\tA euchre deck consists of 24 cards - the 9, 10, jack, "
      ^ "queen, king and ace of each suit.\n\n"
      ^ "Objective:\n\tThe goal is to win at least 3 tricks in a hand between a"
      ^ " partnership. Teams consist of partners sitting opposite each other at"
      ^ " the table (In this version players with ids 0 and 2 are partners on "
      ^ "the green team while players with ids 1 and 3 are partners on the red "
      ^ "team) If the "
      ^ "team that declared the trump suit does not win at least 3 tricks, they"
      ^ " are said to be \"euchred\". Winning all five tricks is called a "
      ^ "\"sweep\".\n\n"
      ^ "Rank of Cards:\n\tThe highest trump is the jack of the trump suit, "
      ^ "called the \"right bower\". The second-highest trump is the jack of "
      ^ "the other suit of the same color, called the \"left bower\". (Ex: "
      ^ "if diamonds are trumps, the right bower is the jack of diamonds and "
      ^ " the left bower is the jack of hearts. The remaining trumps, as well "
      ^ "as the non-trump or \"off suits\" rank as follows: A (high), K, Q, "
      ^ "J (excluding the bowers), 10, 9.\n\n"
      ^ "Scoring:\n\tTeam that declares trump wins 3 or 4 tricks - 1 point"
      ^ "\n\tTeam that declares trump wins 5 tricks - 2 points"
      ^ "\n\tPlayer going alone wins 3 or 4 tricks - 1 point"
      ^ "\n\tPlayer going alone wins 5 tricks - 4 points"
      ^ "\n\tTeam that declares trump wins less than 3 tricks - 2 points for "
      ^ "the other team"
      ^ "\nThe first team to 10 points wins.\n\n"
      ^ "Declaring Trump:\n\tAt the beginning of each turn, a card is flipped "
      ^ "up (known as the up card). Beginning with the player to the left of "
      ^ "the dealer, each player passes or accepts the suit of the up card as"
      ^ " trump. To accept the up suit as trump, a player says \"pick it up\"."
      ^ "When it is the dealers turn, if they choose to pass, the up card gets"
      ^ " flipped over and may not be declared as trump for the current hand. "
      ^ "After this, players are free to declare any suit besides the former up"
      ^ " suit as trump. If all players pass again, the dealer is \"stuck\", "
      ^ "and must declare a suit as trump.\n\n"
      ^ "Playing Alone:\n\tIf the player who declares the trump suit believes "
      ^ "it will be advantageous to play without the help of their partner, "
      ^ "they may declare that they are going \"alone\", in which case their "
      ^ "partner will not play the hand.\n\n"
      ^ "Playing Cards:\n\tThe opening lead is made by the player to the "
      ^ "dealer's left or if that player's partner is going alone, it is made "
      ^ "by the player across from the dealer. If possible, each player must "
      ^ "follow suit to a lead. If unable to follow suit, the player may "
      ^ "play any card they like. A trick is won by the highest card of the "
      ^ "suit led, or, if any trump were played, the trick is won by the "
      ^ "highest trump. The winner of a trick leads next.");
   ANSITerminal.(print_string [magenta] 
                   ("\n\nPress 'Enter' to start the game with default settings "
                    ^ "or type \"setttings\" to adjust the settings.\n"));
   print_string "> ";
   let input = read_line () |> String.trim |> String.lowercase_ascii in
   if input = "settings" then settings Settings.default else Settings.default)

(** [main ()] prompts the user to start and then executes the game.*) 
let main () = 
  ANSITerminal.erase Above;
  ANSITerminal.(print_string [green] "Welcome to Euchre 2D\n");
  print_string 
    ("This is a text-based version of the card game Euchre. You will need 4 " 
     ^ "players to play. Commands can be entered in the following forms:\n" 
     ^ "\tTo pass:\n\t\t\"pass\".\n"
     ^ "\tTo tell the dealer to pick up the up card:\n\t\t\"pick\", or " 
     ^ "\"pick it up\"\n"
     ^ "\tTo drop a card:\n\t\t\"drop\" followed by a valid card phrase\n"
     ^ "\t\t*Valid card phrases follow the format \"Ace of Hearts\" or \"a of" 
     ^ " h\"\n"
     ^ "\tTo declare a suit as trump:\n\t\t\"declare\" followed by a suit\n"
     ^ "\tTo play a card:\n\t\t\"play\" followed by a valid card phrase, or " 
     ^ "just type a valid card phrase\n"
     ^ "\tTo go alone:\n\t\tadd \"alone\" to the end of a pick or declare "
     ^ "command\n"
     ^ "\tTo quit the game:\n\t\t\"quit\"\n");
  ANSITerminal.
    (print_string [magenta] 
       ("If you are not familiar with the rules of euchre, type  \"rules\" and"
        ^ " press 'Enter'. If you would like to adjust the settings of the "
        ^ "game, type \"settings\" and press 'Enter'." 
        ^ "If you would like to start playing, press 'Enter'.\n"));
  print_string "> ";
  let input = read_line () |> String.trim |> String.lowercase_ascii in
  let settings = 
    if input = "rules" then rules () else
    if input = "settings" then settings Settings.default else
      Settings.default in
  turn (State.init settings) pass_message

(** Execute the game.*)
let () = main ()