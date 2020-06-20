(* 
  Testing Approach:
    We primarily used Test Driven Development (black box testing) to test the 
    correctnes of many of the fundamental aspects of our game. Beginning with 
    the deck module, we used unit tests to ensure that we could generate cards 
    and suits from strings,as well as deal cards and hands. The randomness of 
    card dealing was observed through gameplay. We then tested the command 
    module to ensure that we can correctly interpret user inputs. This module 
    was tested entirely using OUnit testing. Testing for the state module was 
    done partially using OUnit testing and partially through gameplay. Basic 
    evaluation functions like suit_of_card and compare_cards were tested (a few
    cases were introduced during test driven development while the rest were 
    derived using glass box testing to test all cases). Additionally, we 
    included some basic tests that ensure exceptions are raised if an illegal 
    move iss attempted in a state, however, since it is difficult to manipulate
    the state in a controlled way without playing the game, the vast majority of
    this testing was done through gameplay in which we strategically chose moves
    in order to test edge cases. Testing of the main and settings modules were 
    also done through gameplay.
    Once the basic functionalities passed all of our designed tests, we focused
    on gameplay testing in order to test rare gamestates and actions, as well as
    ensuring that exception would be thrown and caught if a player attempts a
    move that breaks the rules of the game.

*)

open OUnit2

open Deck

let deck_tests = [
  "suit_from_str spades" >:: (fun _ -> 
      assert_equal (Deck.suit_from_str "spades") (Spades));

  "suit_from_str clubs" >:: (fun _ -> 
      assert_equal (Deck.suit_from_str "clubs") (Clubs));

  "suit_from_str hearts" >:: (fun _ -> 
      assert_equal (Deck.suit_from_str "hearts") (Hearts));

  "suit_from_str diamonds" >:: (fun _ -> 
      assert_equal (Deck.suit_from_str "diamonds") (Diamonds));

  "suit_from_str exception" >:: (fun _ -> 
      assert_raises Deck.InvalidSuit (fun () -> Deck.suit_from_str "spad"));

  "val_from_str nine" >:: (fun _ -> 
      assert_equal (Deck.val_from_str "nine") (Nine));

  "val_from_str ten" >:: (fun _ -> 
      assert_equal (Deck.val_from_str "ten") (Ten));

  "val_from_str jack" >:: (fun _ -> 
      assert_equal (Deck.val_from_str "jack") (Jack));

  "val_from_str queen" >:: (fun _ -> 
      assert_equal (Deck.val_from_str "queen") (Queen));

  "val_from_str king" >:: (fun _ -> 
      assert_equal (Deck.val_from_str "king") (King));

  "val_from_str ace" >:: (fun _ -> 
      assert_equal (Deck.val_from_str "ace") (Ace));

  "val_from_str exception" >:: (fun _ -> 
      assert_raises Deck.InvalidCardValue 
        (fun () -> Deck.val_from_str "eight"));

  "card deal" >:: (fun _ ->
      assert_equal (Deck.deal_card [(Ace, Spades)]) ((Ace, Spades), []));

  "deal hand" >:: (fun _ ->
      assert_equal (List.length (fst (Deck.deal_hand Deck.init))) 5);

]

let command_tests = [
  "Extra whitespace test"  >:: 
  (fun _ -> assert_equal 
      (Command.parse  " declare  hearts ") 
      (Command.Declare(Hearts,false)));
  "Pass test"  >:: (fun _ -> assert_equal (Command.parse  "pass") 
                       (Command.Pass));

  "Pick test" >:: (fun _ -> assert_equal (Command.parse "pick it up") 
                      (Command.Pick(false)));

  "Pick alone test" >:: (fun _ -> 
      assert_equal (Command.parse "pick it up alone") (Command.Pick(true)));

  "Drop test" >:: (fun _ -> assert_equal (Command.parse "drop ace of spades")
                      (Command.Drop(Ace, Spades)));

  "Declare test" >:: (fun _ -> assert_equal (Command.parse "declare hearts")
                         (Command.Declare(Hearts,false)));

  "Declare alone test" >:: (fun _ -> 
      assert_equal (Command.parse "declare hearts alone") 
        (Command.Declare(Hearts,true)));

  "Play test" >::(fun _ -> assert_equal (Command.parse "play jack  of diamonds")
                     (Command.Play(Jack, Diamonds)));

  "Illegal value input test" >:: (fun _ -> 
      assert_raises (Deck.InvalidCardValue) 
        (fun _ -> Command.parse "play eight of clubs"));

  "Illegal suit input" >:: (fun _ -> 
      assert_raises (Deck.InvalidSuit) 
        (fun _ -> Command.parse "declare cloobs"));

  "Illegal command input" >:: (fun _ -> 
      assert_raises (Command.IllegalCommand) 
        (fun _ -> Command.parse "play nine"));

  "Ignores caps and whitespace" >:: (fun _ -> 
      assert_equal (Command.parse "  pLaY    9     of  Clubs    ")
        (Command.parse "play nine of clubs"));

  "quit test" >:: (fun _ -> assert_equal (Command.parse " quit ") 
                      (Command.Quit))

]

open State

let s1 = State.init Settings.default |> State.pass |> State.pass |> State.pass 
         |> State.pass

let state_tests = [
  "suit of card left" >:: (fun _ ->
      assert_equal (suit_of_card (Jack, Spades) (Some Clubs)) (Clubs));

  "suit of card right" >:: (fun _ -> 
      assert_equal (suit_of_card (Jack, Spades) (Some Spades)) (Spades));

  "suit of card trump" >:: (fun _ -> 
      assert_equal (suit_of_card (Ace, Diamonds) (Some Diamonds)) (Diamonds));

  "suit of card off jack" >:: (fun _ -> 
      assert_equal (suit_of_card (Jack, Spades) (Some Diamonds)) (Spades));

  "suit of card off" >:: (fun _ -> 
      assert_equal (suit_of_card (Nine, Hearts) (Some Spades)) (Hearts));

  "compare_cards trump vs trump" >:: (fun _ ->
      assert_equal 
        ((compare_cards (Ace, Clubs) (King, Clubs) Clubs Spades) > 0) 
        true);

  "compare_cards right vs left" >:: (fun _ ->
      assert_equal 
        ((compare_cards (Jack, Spades) (Jack, Clubs) Spades Spades) > 0)
        true);

  "compare_cards left vs lower trump" >:: (fun _ ->
      assert_equal 
        ((compare_cards (Ace, Spades) (Jack, Clubs) Spades Spades) < 0)
        true);

  "compare_cards lead vs trump" >:: (fun _ ->
      assert_equal 
        ((compare_cards (Ace, Clubs) (Jack, Clubs) Spades Clubs) < 0)
        true);

  "compare_cards off vs trump" >:: (fun _ ->
      assert_equal 
        ((compare_cards (Ace, Hearts) (Jack, Clubs) Clubs Spades) < 0)
        true);

  "compare_cards off vs lead" >:: (fun _ ->
      assert_equal 
        ((compare_cards (Ace, Diamonds) (Jack, Clubs) Hearts Clubs) < 0)
        true);

  "compare_cards lead vs lead" >:: (fun _ ->
      assert_equal 
        ((compare_cards (Ace, Clubs) (Jack, Clubs) Hearts Clubs) > 0) 
        true);

  "illegal play from init state" >:: (fun _ ->
      assert_raises (IllegalState) 
        (fun () -> State.play (Ace, Clubs) (State.init Settings.default)));

  "illegal declare from init state" >:: (fun _ ->
      assert_raises (IllegalState) 
        (fun () -> State.declare Hearts true (State.init Settings.default)));

  "illegal drop from init state" >:: (fun _ ->
      assert_raises (IllegalState) 
        (fun () -> State.drop (Nine, Diamonds) (State.init Settings.default)));

  "illegal play during drop" >:: (fun _ ->
      assert_raises (IllegalState) 
        (fun () -> State.pick false (State.init Settings.default) 
                   |> State.play (Nine, Clubs)));

  "illegal pick during drop" >:: (fun _ ->
      assert_raises (IllegalState) 
        (fun () -> State.pick false (State.init Settings.default) 
                   |> State.pick true));

  "illegal pass during drop" >:: (fun _ ->
      assert_raises (IllegalState) 
        (fun () -> State.pick false (State.init Settings.default) 
                   |> State.pass));

  "illegal declare during drop" >:: (fun _ ->
      assert_raises (IllegalState) 
        (fun () -> State.pick false (State.init Settings.default) 
                   |> State.declare Clubs false));

  "illegal pick with no upcard" >:: (fun _ ->
      assert_raises (IllegalState) 
        (fun () -> State.pick false s1));

  "illegal play with no upcard" >:: (fun _ ->
      assert_raises (IllegalState) 
        (fun () -> State.play (Ace, Spades) s1));

  "illegal drop with no upcard" >:: (fun _ ->
      assert_raises (IllegalState) 
        (fun () -> State.drop (Jack, Hearts) s1));

  "illegal drop after declare" >:: (fun _ ->
      assert_raises (IllegalState) 
        (fun () -> State.drop (Ace, Clubs) (State.declare Hearts false s1)));

  "init state game is not over" >:: (fun _ ->
      assert_equal (State.game_over (State.init Settings.default)) 
        (None));

]

let suite = "euchre test suite" >::: List.flatten
              [deck_tests; command_tests; state_tests;]

let _ = run_test_tt_main suite
