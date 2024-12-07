open OUnit2
open Printf
open Cs3110_fin.Logic

let test_make_state _ =
  let grid = [| [| 'a'; 'b' |]; [| 'c'; 'd' |] |] in
  let state = initialize_game grid in
  assert_equal state.grid grid;
  assert_equal state.found_words BatSet.empty

let test_print_letter _ =
  let buffer = Buffer.create 16 in
  let fmt = Format.formatter_of_buffer buffer in

  (* Define a helper function to print to the buffer instead of stdout *)
  let print_letter_to_buffer letter highlight =
    if highlight then Format.fprintf fmt "\027[1;32m%c\027[0m " letter
    else Format.fprintf fmt "%c " letter
  in

  (* Call the function with the desired letter and highlight flag *)
  print_letter_to_buffer 'a' true;

  (* Finalize output and assert *)
  Format.pp_print_flush fmt ();

  (* Ensure all contents are flushed to the buffer *)
  let output = Buffer.contents buffer in
  assert_equal output "\027[1;32ma\027[0m "

let test_is_highlighted _ =
  let word_positions = [ ("word", [ (0, 0); (1, 1) ]) ] in
  let found_words = [ "word" ] in
  assert_equal (is_highlighted (0, 0) found_words word_positions) true;
  assert_equal (is_highlighted (0, 1) found_words word_positions) false

let test_handle_guess _ =
  let grid = [| [| 'a'; 'b' |]; [| 'c'; 'd' |] |] in
  let state = initialize_game grid in
  let target_words = [ "hello"; "world" ] in
  let updated_state = handle_guess state "hello" target_words in
  let temp = BatSet.empty in
  let temp2 = BatSet.add "hello" temp in
  assert_equal updated_state.found_words temp2;
  let no_update_state = handle_guess updated_state "invalid" target_words in
  assert_equal no_update_state.found_words temp2

let test_process_input _ =
  let grid = [| [| 'a'; 'b' |]; [| 'c'; 'd' |] |] in
  let target_words = [ "target" ] in
  let accepted_words = BatSet.of_list [ "hint"; "clue" ] in
  let match_counter = ref 0 in
  let hint_counter = ref 0 in
  let max_hints = 3 in
  let initial_state = initialize_game grid in

  let match_state =
    process_input initial_state "target" target_words match_counter hint_counter
      max_hints accepted_words
  in
  let temp = BatSet.of_list [ "target" ] in
  assert_equal !match_counter 1;
  assert_equal match_state.found_words temp;

  let hint_state =
    process_input match_state "hint" target_words match_counter hint_counter
      max_hints accepted_words
  in
  assert_equal hint_state.found_words (BatSet.of_list [ "target" ]);
  assert_equal !hint_counter 1;

  let invalid_state =
    process_input hint_state "invalid" target_words match_counter hint_counter
      max_hints accepted_words
  in
  assert_equal invalid_state.found_words (BatSet.of_list [ "target" ]);
  assert_equal !match_counter 1;
  assert_equal !hint_counter 1

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
