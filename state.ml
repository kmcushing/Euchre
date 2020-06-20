open Deck

type value = Deck.value
type suit = Deck.suit
type card = Deck.card
type is_alone = bool

(* The type [player_id] represents the id for a player in the game. *)
type player_id = int

(** [player] is the type representing a player in a game. *)
type player = {id : int; hand : card list}

(** [board] is the type representing the cards on the game board. *)
type board = card list

type t = {
  players : player list; 
  board : board;
  up_card : card option; 
  score : (int * int); 
  trump : suit option;
  tricks : (int * int);
  current_player_id : player_id;
  suit_lead : suit option;
  dealer_id : player_id;
  up_suit : suit option;
  declaring_player : (player_id * is_alone) option;
  settings : Settings.t
}

exception IllegalState

(** [players_with_new_hands p d] is the player list [p] with each player having
    a newly dealt hand from the deck [d]. *)
let rec players_with_new_hands players d : player list = 
  match players with
  | [] -> []
  | p :: t -> 
    (let (h, d2) = Deck.deal_hand d in
     {p with hand = h} :: players_with_new_hands t d2)

(** [trump_suit s] is the trump suit in state [s].
    Raises [Failure] if no trump suit has been declared. *)
let trump_suit s = 
  match s.trump with
  | Some suit -> suit
  | None -> failwith "No Trump Suit"

(** [suit_lead s] is the suit lead at the beginning of the current trick in 
    state [s].
    Raises [Failure] if no suit hass been lead. *)
let suit_lead s = 
  match s.suit_lead with
  | Some suit -> suit
  | None -> failwith "No Lead Suit"

(** [suit_of_same_color s] is the suit of the same color as suit [s].
    Ex: suit_of_same_color Spades is Clubs. *)
let suit_of_same_color s =
  match s with
  | Clubs -> Spades
  | Diamonds -> Hearts
  | Hearts -> Diamonds
  | Spades -> Clubs

let suit_of_card (c : card) (trump : suit option) = 
  match c with
  | (v,s) -> 
    if v = Jack then 
      (match trump with
       | Some suit ->
         if s = suit_of_same_color suit then
           suit
         else
           s
       | None -> s)
    else
      s

(** [next_id id n] is the id of the player [n] players after the player with id
    [id] at a table with 4 players. *)
let next_id id n = 
  let id' = (id + n) mod 4 in if id' >= 0 then id' else (id' + 4)

(** [next_player_from id s] is the player whos turn comes after the player 
    with id [id] in state [s]. *)
let next_player_from id s = 
  match s.declaring_player with
  | Some (loner, true) -> 
    if next_id loner 1 = id then 
      next_id id 2 
    else 
      next_id id 1
  | _ -> next_id id 1

(** [next_player s] is the id of the player whos turn comes after the current 
    player in state [s]. *)
let next_player s = 
  next_player_from s.current_player_id s

(** [suit_is_in_hand h s t] is true if hand [h] contains at least one card of 
    suit [s] in a state where the trump suit has been declared as [t]. *)
let rec suit_is_in_hand h suit trump = 
  match h with
  | [] -> false
  | c :: t -> 
    if suit_of_card c trump = suit then 
      true 
    else 
      suit_is_in_hand t suit trump

(** [player_from_id p id] is the player in player list [p] with id [id].
    Raises [Failure] if no player in [p] has id [id]. *)
let rec player_from_id players id = 
  match players with
  | [] -> failwith "Illegal Player"
  | p :: t -> if p.id = id then p else player_from_id t id

(** [hand_without_card c h] is hand [h] after removing card [c] from it.
    Raises [IllegalState] if card [c] is not in hand [h]. *)
let rec hand_without_card c h = 
  match h with
  | [] -> raise IllegalState
  | c' :: t -> if c' = c then t else c' :: hand_without_card c t

(** [player_without_card p c] is the player [p] without the card [c] in their
    hand.
    Raises [IllegalState] if card [c] is not in player [p]'s hand. *)
let player_without_card player c = 
  {player with hand = hand_without_card c player.hand}

(** [players_update_player_by_id players id p] is the player list [players],
    replacing the player with id [id] with the player [p]. 
    Raises [Failure] if there is no player with id [id] in players. *)
let rec players_update_player_by_id players id p' = 
  match players with
  | [] -> []
  | p :: t -> 
    if p.id = id then 
      p' :: t 
    else 
      p :: players_update_player_by_id t id p'

(** [print_cards c] prints the card list [c] in string format. *)
let rec print_cards cards =
  match cards with
  | [] -> ()
  | c :: t -> (c |> Deck.card_to_string) ^ "\t" |> print_string; print_cards t

(** [display_line l] prints out the values of the 
    (string * ANSITerminal list) list[l] according to their designated
    styles. *)
let rec display_line l = 
  match l with
  | [] -> print_string "\n";
  | (str, style) :: t -> (ANSITerminal.print_string style str; 
                          print_string "\t"; display_line t;)

(** [display s] prints out the values of each element of [s] according to 
    their designated styles. *)
let rec display strings_and_styles = 
  match strings_and_styles with
  | [] -> print_string "\n"
  | l :: t -> display_line l; display t

(** [concat_string_and_styles c s] is the 2-D list reulting from combining 
    each line of the visual representation of card [c] with the corresponding
    line of the visual representation of [s]. *)
let rec concat_strings_and_styles c strings_and_styles = 
  match c with
  | (c1 :: c2 :: c3 :: c4 :: c5 :: c6 :: [], style) ->
    (match strings_and_styles with
     | l1 :: l2 :: l3 :: l4 :: l5 :: l6 :: [] -> 
       [(c1,style)::l1; (c2,style)::l2; (c3,style)::l3; (c4,style)::l4;
        (c5,style)::l5; (c6,style)::l6]
     | [[]] -> [[(c1,style)]; [(c2,style)]; [(c3,style)]; [(c4,style)];
                [(c5,style)]; [(c6,style)]]
     | _ -> failwith "Illegal card returned")
  | _ -> failwith "Illegal Card Returned"

(** [diplay_cards c s settings] prints a representation of the card list [c] 
    added to the (string * ANSITerminal list)list [s] according to the
    settings [settings]. *)
let rec display_cards cards strings_and_styles settings = 
  match cards with
  | [] -> display strings_and_styles
  | c :: t -> let  card_display = Deck.card_to_strings_and_styles c settings in
    display_cards t 
      (concat_strings_and_styles card_display strings_and_styles) settings

(** [index l c i] is the value of the index of [c] in list [l] added to [i]. *)
let rec index lst c i = 
  match lst with
  | [] -> -1
  | h :: t -> if h = c then i else index t c (i + 1)

(** [ordered_card_list t l] is a list of cards of the suits [t] (the trump suit)
    and [l] (the suit lead at the beginning of the most recent trick) in ranked
    order from lowest to highest. *)
let ordered_card_list trump lead = 
  let t = [(Queen, lead); (King, lead); (Ace, lead); (Nine, trump); 
           (Ten, trump); (Queen, trump); (King, trump); (Ace, trump);
           (Jack, (suit_of_same_color trump)); (Jack, trump)] in
  (Nine, lead) :: (Ten, lead) :: 
  (if lead = trump || lead = suit_of_same_color trump then
     t
   else
     (Jack, lead) :: t)

let compare_cards c1 c2 trump lead =
  let l = ordered_card_list trump lead in
  let index_of_c1 = index l c1 0 in
  let index_of_c2 = index l c2 0 in
  index_of_c1 - index_of_c2

(** [nth_next_player s n] is the result of calling [next_player s] n times. *)
let rec nth_next_player s n = 
  let x = s.current_player_id in
  match n with
  | 0 -> x
  | _ -> nth_next_player {s with current_player_id = next_player s} (n - 1)

(** [winner_of_trick c id s b] is the id of the player who played the best 
    card between the cards in [board] and the card [best_card] in state [s] 
    where [id] is the id of the player who played the card [best_card]. *)
let rec winner_of_trick best_card id s board = 
  match board with
  | [] -> id
  | c :: t -> 
    if (compare_cards c best_card (trump_suit s) (suit_lead s)) > 0 then
      let new_winner_id = (match s.declaring_player with
          | Some (_, true) -> nth_next_player s (2-(List.length t))
          | _ -> next_id s.current_player_id (3 - List.length t)) in 
      winner_of_trick c new_winner_id s t
    else
      winner_of_trick best_card id s t

(** [team_of_player_id i] is the number representing the team that the player 
    with id [id] is on. *)
let team_of_player_id id = 
  (id mod 2) + 1

let game_over s = 
  match s.score with
  | (x,y) -> if x >= Settings.get_winning_score s.settings then 
      Some 1 
    else if y >= Settings.get_winning_score s.settings then
      Some 2
    else
      None

(** [deal_cards s] is the state resulting from dealing new cards to all 
    players and dealing a new up card in state [s]. *)
let deal_cards s = 
  let (up, d) = Deck.deal_card Deck.init in
  {s with players = players_with_new_hands s.players d; up_card = Some up;
          current_player_id = next_id s.dealer_id 1; 
          up_suit = Some (suit_of_card up None)}

(** [new_score s] is the state resulting from updating the score of state [s] 
    based on the winner of the previous trick. *)
let new_score s =
  let (t1,t2) = s.tricks in
  let (sc1,sc2) = s.score in
  match s.declaring_player, (t1>t2) with
  |Some (id,alone), true -> 
    if (team_of_player_id id) = 1 then
      if t1=5 then
        if alone then (sc1 + 4, sc2) 
        else (sc1+2,sc2)
      else (sc1+1,sc2)
    else (sc1+2,sc2)
  |Some (id,alone), false -> 
    if (team_of_player_id id) = 2 then
      if t2=5 then
        if alone then (sc1, sc2 + 4) 
        else (sc1,sc2+2)
      else (sc1, sc2 + 1)
    else (sc1,sc2+2)
  |_ -> raise IllegalState 

(** [end_of_hand s] is the state resulting from updating [s] if it represents 
    a state where a hand has ended. If the current hand is not over, returns 
    [s] unchanged. *)
let end_of_hand s = 
  let (t1,t2) = s.tricks in 
  if t1 + t2 = 5 then
    {s with board = []; score = new_score s; tricks = (0,0); 
            current_player_id = next_id s.dealer_id 2; suit_lead = None; 
            dealer_id = next_id s.dealer_id 1; up_suit = None;
            declaring_player = None;
            trump = None} 
    |> deal_cards
  else s

(** [end_of_trick s] is the state resulting from updating [s] if it represents
    a state where a trick has ended. If the current trick is not over, returns
    [s] unchanged. *)
let end_of_trick s = 
  let nplayers = (match s.declaring_player with
      | Some (_,true) -> 3
      | _  -> 4) in 
  if List.length s.board = nplayers
  then
    let winner_id = winner_of_trick (List.nth s.board (nplayers - 1)) 
        s.current_player_id s (List.rev s.board) in
    end_of_hand {s with tricks = (match s.tricks with
        |(t1,t2) -> if winner_id |> team_of_player_id = 1 
          then (t1+1,t2)
          else (t1,t2+1)); board = []; 
           current_player_id = winner_id; 
           suit_lead = None}
  else 
    s

(** [empty] represents a new game state before cards have been dealt. *)
let empty = 
  Random.self_init ();
  let d = Random.int 4 in
  { 
    players = [{id = 0; hand = []}; {id = 1; hand = []}; {id = 2; hand = []};
               {id = 3; hand = []}]; 
    board = [];
    up_card = None; 
    score = (0,0); 
    trump = None;
    tricks = (0,0);
    current_player_id = next_id d 1;
    suit_lead = None;
    dealer_id = d;
    up_suit = None;
    declaring_player = None;
    settings = Settings.default
  }

let settings s = s.settings

(** [set_settings settings s] is the game state [s] with settings set to 
    [settings]. *)
let set_settings settings s = 
  {s with settings = settings}

let init settings = deal_cards empty |> set_settings settings

let play c s = 
  let s' = {s with players = (players_update_player_by_id s.players 
                                s.current_player_id 
                                (player_without_card 
                                   (player_from_id s.players 
                                      s.current_player_id) c)); 
                   board = c :: s.board; 
                   current_player_id = next_player s} in
  match s.trump, s.up_card with
  | Some suit, None ->
    (match s.suit_lead with
     | Some sl -> 
       (if suit_is_in_hand (player_from_id s.players s.current_player_id).hand 
           sl s.trump then
          if suit_of_card c s.trump = sl then
            end_of_trick s'
          else
            raise IllegalState
        else
          end_of_trick s')
     | None -> end_of_trick {s' with suit_lead = Some (suit_of_card c s.trump)})
  | _, _ -> raise IllegalState

let pick (alone : is_alone) s = 
  match s.up_card with
  | Some c -> 
    if s.up_suit = None then 
      raise IllegalState 
    else
      {s with trump = s.up_suit; 
              current_player_id = s.dealer_id;
              up_suit = None; 
              declaring_player = Some (s.current_player_id, alone)}
  | None -> raise IllegalState

(** [new_player_from_drop p c u d] is the player list resulting from the player
    with id [dealer_id] in [players] picking up the card up card [u] and 
    dropping card [c]. 
    Note: technically it is a legal move to drop [u], however, it is rarely, 
    if ever, advantageous to do so. *)
let rec new_players_from_drop players c up_card dealer_id = match players with
  | [] -> []
  | {id = id; hand = h} :: t -> if id = dealer_id then 
      {id = id; hand = hand_without_card c (up_card::h)}::
      (new_players_from_drop t c up_card dealer_id) 
    else
      {id = id; hand = h}::(new_players_from_drop t c up_card dealer_id)

let drop c s = 
  if s.up_suit = None && s.current_player_id = s.dealer_id then
    match s.up_card with
    | Some uc -> 
      {s with players = new_players_from_drop s.players c uc s.dealer_id;
              up_card = None; 
              current_player_id = next_player_from s.dealer_id s}
    | None -> raise IllegalState
  else raise IllegalState

let pass s = 
  let s' = {s with current_player_id = next_id s.current_player_id 1} in
  if s.up_suit <> None then
    match s.up_card with
    | Some c -> 
      if s.current_player_id = s.dealer_id then
        {s' with up_card = None}
      else
        s'
    | None -> 
      if s.current_player_id = s.dealer_id then
        if Settings.get_stick_the_dealer s.settings then
          raise IllegalState
        else
          deal_cards {s with dealer_id = next_id s.dealer_id 1;
                             current_player_id = next_id s.dealer_id 2}
      else 
        s'
  else raise IllegalState

let declare suit (alone : is_alone) s = 
  match s.trump with
  | Some s -> raise IllegalState
  | None -> 
    (match s.up_card with
     | Some c -> raise IllegalState
     | None -> 
       (match s.up_suit with 
        | None -> raise IllegalState
        | Some us -> if suit = us then
            raise IllegalState
          else
            let s' = {s with trump = Some (suit);
                             declaring_player = 
                               Some (s.current_player_id, alone)}
            in
            {s' with current_player_id = 
                       next_player_from s'.dealer_id s'}))

(** the number of lines that a hand will need to be displayed *)
let n_lines_for_hand_display = 10
(** the number of lines that the board will need to be displayed *)
let n_lines_for_board_display = 7
(** the number of lines that an upcard will need to be displayed *)
let n_lines_for_upcard_display = 8

let print_hand s =
  ANSITerminal.scroll (-n_lines_for_hand_display);
  ANSITerminal.move_bol ();
  print_endline "Your hand:";
  display_cards (player_from_id s.players s.current_player_id).hand [[]] 
    s.settings;
  print_string "\n"

(** [print_players s] prints the players in state [s] in a text format. *)
let print_players s =
  let is_alone_str id = match s.declaring_player with 
    | Some (i, true)-> if i = id then "alone" else "     "
    | _ -> "     " in
  let is_dealer_str id = match s.dealer_id with 
    | i -> if i = id then "dealer" else "      " in
  let is_current_player_str id = if s.current_player_id = id 
    then "<< CURRENT PLAYER\n" 
    else "\n"in
  let print_status id c =
    let formatted_str = Printf.sprintf "Player %i: %s  %s  %s" id 
        (is_alone_str id) (is_dealer_str id)
        (is_current_player_str id) in
    ANSITerminal.(print_string [c] formatted_str) in
  Printf.printf "          Alone? Dealer?\n";
  print_status 0 (Foreground Green);
  print_status 1 (Foreground Red);
  print_status 2 (Foreground Green);
  print_status 3 (Foreground Red)

let print_without_hand s = 
  print_string "Score:\n";
  (match s.score with
   |(s1, s2) -> 
     ANSITerminal.(print_string [green] (Printf.sprintf "Green Team: %i\n" s1));
     ANSITerminal.(print_string [red] (Printf.sprintf "Red Team: %i\n" s2)));
  print_string "Tricks:\n";
  (match s.tricks with
   |(t1, t2) -> 
     ANSITerminal.(print_string [green] (Printf.sprintf "Green Team: %i\n" t1));
     ANSITerminal.(print_string [red] (Printf.sprintf "Red Team: %i\n" t2)));
  print_string "\nUp card: " ;
  (match s.up_card with
   | Some card -> print_string "\n"; display_cards [card] [[]] s.settings;
   | None -> 
     print_string "NO UPCARD";
     ANSITerminal.scroll n_lines_for_upcard_display;
     ANSITerminal.move_bol ());
  print_string "Board: \n";
  (match s.board with
   | [] -> (ANSITerminal.scroll n_lines_for_board_display;
            ANSITerminal.move_bol ())
   | h::t -> display_cards s.board [[]] s.settings);
  print_string "\n\n";
  print_players s;
  (match s.trump with
   | Some suit -> print_string ("\nTrump suit:    " ^ 
                                (suit |> Deck.suit_to_string 
                                 |> String.capitalize_ascii));
   | None -> print_string "\nTrump suit:    NONE");
  print_string "\n";
  ANSITerminal.scroll n_lines_for_hand_display; 