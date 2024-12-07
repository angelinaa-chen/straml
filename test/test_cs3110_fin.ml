open OUnit2
open Printf
open Cs3110_fin.Logic

let test_make_state _ =
  let grid = [| [| 'a'; 'b' |]; [| 'c'; 'd' |] |] in
  let theme = "Fall Fun" in
  let state = initialize_game grid theme in

  assert_equal state.grid grid;
  assert_equal state.found_words BatSet.empty;
  assert_equal state.guessed_words BatSet.empty;
  assert_equal state.theme theme

let test_print_letter _ =
  let buffer = Buffer.create 16 in
  let fmt = Format.formatter_of_buffer buffer in

  let print_letter_to_buffer letter highlight =
    if highlight then Format.fprintf fmt "\027[1;32m%c\027[0m " letter
    else Format.fprintf fmt "%c " letter
  in
  print_letter_to_buffer 'a' true;

  Format.pp_print_flush fmt ();
  let output = Buffer.contents buffer in
  assert_equal output "\027[1;32ma\027[0m "

let test_is_highlighted _ =
  let word_positions = [ ("word", [ (0, 0); (1, 1) ]) ] in
  let found_words = [ "word" ] in
  assert_equal (is_highlighted (0, 0) found_words word_positions) true;
  assert_equal (is_highlighted (0, 1) found_words word_positions) false

let test_handle_guess _ =
  let grid = [| [| 'a'; 'b' |]; [| 'c'; 'd' |] |] in
  let theme = "Fall Fun" in
  let state = initialize_game grid theme in
  let target_words = [ "hello"; "world" ] in

  let updated_state = handle_guess state "hello" target_words in
  let expected_found_words = BatSet.add "hello" state.found_words in
  assert_equal true
    (BatSet.equal updated_state.found_words expected_found_words);

  let no_update_state = handle_guess updated_state "error" target_words in
  assert_equal true
    (BatSet.equal no_update_state.found_words expected_found_words)

let test_process_input _ =
  let grid = [| [| 'a'; 'b' |]; [| 'c'; 'd' |] |] in
  let theme = "Fall Fun" in
  let target_words = [ "target" ] in
  let accepted_words = BatSet.of_list [ "hint"; "clue" ] in
  let match_counter = ref 0 in
  let hint_counter = ref 0 in
  let max_hints = 3 in
  let initial_state = initialize_game grid theme in

  let match_state =
    process_input initial_state "target" target_words match_counter hint_counter
      max_hints accepted_words
  in
  let expected_found_words = BatSet.add "target" initial_state.found_words in
  assert_equal !match_counter 1;
  assert_equal match_state.found_words expected_found_words;

  let hint_state =
    process_input match_state "hint" target_words match_counter hint_counter
      max_hints accepted_words
  in
  assert_equal !hint_counter 1;
  assert_equal hint_state.found_words expected_found_words;

  let invalid_state =
    process_input hint_state "invalid" target_words match_counter hint_counter
      max_hints accepted_words
  in
  assert_equal !match_counter 1;
  assert_equal !hint_counter 1;
  assert_equal invalid_state.found_words expected_found_words

let suite =
  "TestGame"
  >::: [
         "test_make_state" >:: test_make_state;
         "test_print_letter" >:: test_print_letter;
         "test_is_highlighted" >:: test_is_highlighted;
         "test_handle_guess" >:: test_handle_guess;
         "test_process_input" >:: test_process_input;
       ]

let () = run_test_tt_main suite
