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

let test_make_state2 _ =
  let grid =
    [| [| 'a'; 'b'; 'c' |]; [| 'd'; 'e'; 'f' |]; [| 'g'; 'h'; 'i' |] |]
  in
  let theme = "alphabet" in
  let state = initialize_game grid theme in

  assert_equal state.grid grid;
  assert_equal state.found_words BatSet.empty;
  assert_equal state.guessed_words BatSet.empty;
  assert_equal state.theme theme

(* test terminal printing with color*)
let test_print_letter _ =
  let buffer = Buffer.create 16 in
  let fmt = Format.formatter_of_buffer buffer in

  let print_letter_to_buffer letter highlight =
    if highlight then Format.fprintf fmt "\027[1;32m%c\027[0m " letter
    else Format.fprintf fmt "%c\n " letter
  in
  print_letter_to_buffer 'a' true;

  Format.pp_print_flush fmt ();
  let output = Buffer.contents buffer in
  assert_equal output "\027[1;32ma\027[0m "

let test_is_highlighted _ =
  let word_positions = [ ("word", [ (0, 0); (1, 1); (0, 1); (1, 2) ]) ] in
  let found_words = [ "word" ] in
  assert_equal (is_highlighted (0, 0) found_words word_positions) true;
  assert_equal (is_highlighted (1, 1) found_words word_positions) true;
  assert_equal (is_highlighted (0, 1) found_words word_positions) true;
  assert_equal (is_highlighted (1, 2) found_words word_positions) true;
  assert_equal (is_highlighted (1, 0) found_words word_positions) false;
  assert_equal (is_highlighted (-1, -1) found_words word_positions) false;
  assert_equal (is_highlighted (0, -1) found_words word_positions) false

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

let test_is_word_in_grid _ =
  let grid =
    [| [| 'A'; 'B'; 'C' |]; [| 'D'; 'E'; 'F' |]; [| 'G'; 'H'; 'I' |] |]
  in
  assert_bool "Word should be found" (is_word_in_grid grid "ABC");
  assert_bool "Word should be found" (is_word_in_grid grid "ADH");
  assert_bool "Word should be found" (is_word_in_grid grid "IFC");
  assert_bool "Word should be found" (is_word_in_grid grid "EGD");
  assert_bool "Word should not be found" (not (is_word_in_grid grid "XYZ"));
  assert_bool "Word should not be found" (not (is_word_in_grid grid "ACI"));
  assert_bool "Word should not be found" (not (is_word_in_grid grid "BDF"));
  assert_bool "Word should not be found" (not (is_word_in_grid grid "IHB"))

let test_alr_guessed _ =
  (* Initial guessed words set *)
  let guessed_words = BatSet.of_list [ "matthew"; "amy"; "angie" ] in

  (* Guess is new, should be added *)
  let new_guess = "date" in
  let result = alr_guessed new_guess guessed_words in
  assert_equal (BatSet.of_list [ "matthew"; "amy"; "angie"; "date" ]) result;

  let new_guess2 = "" in
  let result2 = alr_guessed new_guess2 result in
  assert_equal
    (BatSet.of_list [ "matthew"; "amy"; "angie"; "date"; "" ])
    result2;

  (* Guess is already in the set, should remain unchanged *)
  let existing_guess = "amy" in
  let result = alr_guessed existing_guess guessed_words in
  assert_equal (BatSet.of_list [ "matthew"; "amy"; "angie" ]) result;

  let existing_guess2 = "" in
  let result2 = alr_guessed existing_guess2 result2 in
  assert_equal
    (BatSet.of_list [ "matthew"; "amy"; "angie"; "date"; "" ])
    result2

let test_get_letter _ =
  (* Highlight with mode 1 (green) *)
  let result = get_letter 'a' true 1 in
  assert_equal "<span foreground=\"green\">a </span>" result;

  (* Highlight with mode 2 (blue) *)
  let result = get_letter 'c' true 2 in
  assert_equal "<span foreground=\"blue\">c </span>" result;

  (* Highlight with mode 3 (yellow) *)
  let result = get_letter 'd' true 3 in
  assert_equal "<span foreground=\"yellow\">d </span>" result;

  (* Highlight with invalid mode (should not highlight) *)
  let result = get_letter 'e' true 4 in
  assert_equal "e " result;

  let result = get_letter 'k' true 4 in
  assert_equal "k " result;

  let result = get_letter 'z' true 4 in
  assert_equal "z " result;

  let result = get_letter ' ' true 4 in
  assert_equal "  " result;

  (* No highlight (highlight = false) *)
  let result = get_letter 'f' false 0 in
  assert_equal "f " result

(* Helper function to capture the output of a printed statement *)
let capture_output f =
  let output = open_out "/tmp/test_output.txt" in
  let saved_stdout = Unix.dup Unix.stdout in
  Unix.dup2 (Unix.descr_of_out_channel output) Unix.stdout;
  f ();
  flush stdout;
  Unix.dup2 saved_stdout Unix.stdout;
  close_out output;
  let lines = ref [] in
  let ic = open_in "/tmp/test_output.txt" in
  try
    while true do
      lines := input_line ic :: !lines
    done;
    []
  with End_of_file ->
    close_in ic;
    List.rev !lines

let test_print_letter_yellow _ =
  let result = capture_output (fun () -> print_letter_yellow 'a' true) in
  assert_equal [ "\027[1;33ma\027[0m " ] result;

  let result = capture_output (fun () -> print_letter_yellow ' ' true) in
  assert_equal [ "\027[1;33m\027[0m " ] result;

  let result = capture_output (fun () -> print_letter_yellow 'a' false) in
  assert_equal [ "a " ] result

(* Test case for print_letter_green *)
let test_print_letter_green _ =
  (* Should print in green (escape sequence) *)
  let result = capture_output (fun () -> print_letter_green 'b' true) in
  assert_equal [ "\027[1;32mb\027[0m " ] result;

  (* Print normally without escape sequences *)
  let result = capture_output (fun () -> print_letter_green 'b' false) in
  assert_equal [ "b " ] result

(* Test case for print_letter_blue *)
let test_print_letter_blue _ =
  (*Print in blue (escape sequence) *)
  let result = capture_output (fun () -> print_letter_blue 'c' true) in
  assert_equal [ "\027[1;34mc\027[0m " ] result;

  (* Print normally without escape sequences *)
  let result = capture_output (fun () -> print_letter_blue 'c' false) in
  assert_equal [ "c " ] result

let test_find_index _ =
  let arr = [| 1; 2; 3; 2; 4 |] in
  let result = find_index arr 2 3 in
  assert_equal None result;

  let arr = [| 1; 2; 3; 2; 4 |] in
  let result = find_index arr 2 1 in
  assert_equal (Some 3) result;

  let arr = [| 1; 1; 1; 1; 1 |] in
  let result = find_index arr 1 0 in
  assert_equal (Some 0) result;
  let result2 = find_index arr 1 4 in
  assert_equal (Some 4) result2;
  let result2 = find_index arr 6 2 in
  assert_equal None result2;

  (* Element is not present in the array *)
  let arr = [| 1; 2; 3; 4 |] in
  let result = find_index arr 5 1 in
  assert_equal None result;
  let result2 = find_index arr 0 1 in
  assert_equal None result2;
  let result3 = find_index arr 2 1 in
  assert_equal None result3;
  let result4 = find_index arr 1 3 in
  assert_equal None result4;

  (* Empty array, element is not found *)
  let arr = [||] in
  let result = find_index arr 1 1 in
  assert_equal None result;
  let result = find_index arr 0 0 in
  assert_equal None result;

  (* Element occurs only once, and asking for the second instance should return
     None *)
  let arr = [| 10 |] in
  let result = find_index arr 10 0 in
  assert_equal (Some 0) result;
  let result = find_index arr 10 2 in
  assert_equal None result

let test_load_words _ =
  (* Load words from a valid file *)
  let content = "matthew\namy\nfalak\n" in
  let filename = Filename.temp_file "test" ".txt" in
  let oc = open_out filename in
  output_string oc content;
  close_out oc;

  let result = load_words filename in
  let expected = BatSet.of_list [ "matthew"; "amy"; "falak" ] in

  assert_equal result expected;

  let content2 = "angie" in
  let filename2 = Filename.temp_file "test2" ".txt" in
  let oc2 = open_out filename2 in
  output_string oc2 content2;
  close_out oc2;

  let result = load_words filename2 in
  let expected = BatSet.of_list [ "angie" ] in

  assert_equal result expected;

  (* Load words with duplicates from file (duplicates should be ignored) *)
  let content = "matthew\namy\nmatthew\nfalak\namy\n" in
  let filename = Filename.temp_file "test" ".txt" in
  let oc = open_out filename in
  output_string oc content;
  close_out oc;

  let content = "angie\nangie\nangie\nangie\n" in
  let filename = Filename.temp_file "test" ".txt" in
  let oc = open_out filename in
  output_string oc content;
  close_out oc;
  let result = load_words filename in
  let expected = BatSet.of_list [ "angie" ] in
  assert_equal result expected;

  (* Load an empty file (should return an empty set) *)
  let content = "" in
  let filename = Filename.temp_file "test" ".txt" in
  let oc = open_out filename in
  output_string oc content;
  close_out oc;

  let result = load_words filename in
  let expected = BatSet.empty in
  assert_equal result expected;

  (* Load a file with mixed case words (should convert to lowercase) *)
  let content = "Matthew\nAmy\nFalak\n" in
  let filename = Filename.temp_file "test" ".txt" in
  let oc = open_out filename in
  output_string oc content;
  close_out oc;

  let result = load_words filename in
  let expected = BatSet.of_list [ "matthew"; "amy"; "falak" ] in
  assert_equal result expected;

  let content = "ANGIE\nANGELINA\nCHENGELINA\n" in
  let filename = Filename.temp_file "test" ".txt" in
  let oc = open_out filename in
  output_string oc content;
  close_out oc;

  let result = load_words filename in
  let expected = BatSet.of_list [ "angie"; "angelina"; "chengelina" ] in
  assert_equal result expected

let test_word_to_highlight _ =
  (* No words found yet, should return the first word from target words *)
  let state =
    {
      grid = [| [| 'a'; 'b'; 'c' |]; [| 'd'; 'e'; 'f' |]; [| 'g'; 'h'; 'i' |] |];
      found_words = BatSet.empty;
      guessed_words = BatSet.empty;
      theme = "Animals";
    }
  in
  let target_words = [ "cat"; "dog"; "fish" ] in
  let result = word_to_highlight state target_words in
  match result with
  | Some word -> Printf.printf "Test 1 passed: %s\n" word
  | None -> (
      Printf.printf "Test 1 failed\n";

      (* Test case 2: One word found, should return the next unguessed word *)
      let state =
        {
          grid =
            [| [| 'a'; 'b'; 'c' |]; [| 'd'; 'e'; 'f' |]; [| 'g'; 'h'; 'i' |] |];
          found_words = BatSet.add "cat" BatSet.empty;
          guessed_words = BatSet.empty;
          theme = "Animals";
        }
      in
      let result = word_to_highlight state target_words in
      match result with
      | Some word when word = "dog" -> Printf.printf "Test 2 passed: %s\n" word
      | _ -> (
          Printf.printf "Test 2\n   failed\n";

          (* Test case 3: All words found, should return None *)
          let state =
            {
              grid =
                [|
                  [| 'a'; 'b'; 'c' |]; [| 'd'; 'e'; 'f' |]; [| 'g'; 'h'; 'i' |];
                |];
              found_words = BatSet.of_list [ "cat"; "dog"; "fish" ];
              guessed_words = BatSet.empty;
              theme = "Animals";
            }
          in
          let result = word_to_highlight state target_words in
          match result with
          | None -> Printf.printf "Test 3 passed\n"
          | Some _ -> (
              Printf.printf "Test 3 failed\n";

              (* Test case 4: Target words empty, should return None *)
              let state =
                {
                  grid =
                    [|
                      [| 'a'; 'b'; 'c' |];
                      [| 'd'; 'e'; 'f' |];
                      [| 'g'; 'h'; 'i' |];
                    |];
                  found_words = BatSet.empty;
                  guessed_words = BatSet.empty;
                  theme = "Animals";
                }
              in
              let result = word_to_highlight state [] in
              match result with
              | None -> Printf.printf "Test 4 passed\n"
              | Some _ -> Printf.printf "Test 4\n   failed\n")))

(* Utility function to capture the output of a function that uses
   print_endline *)
let capture_stdout f =
  let old_stdout = Unix.dup Unix.stdout in
  let pipe_r, pipe_w = Unix.pipe () in
  Unix.dup2 pipe_w Unix.stdout;
  f ();
  Unix.dup2 old_stdout Unix.stdout;
  let result = really_input_string (Unix.in_channel_of_descr pipe_r) in
  Unix.close pipe_r;
  Unix.close pipe_w;
  result

let test_hint_revealer _ =
  (* Initialize GTK *)
  let _ = GMain.init () in

  (* Create a real grid_box *)
  let window = GWindow.window ~title:"Test" ~width:200 ~height:200 () in
  let grid_box = GPack.vbox ~packing:window#add () in

  let grid = [| [| 'a'; 'b' |]; [| 'c'; 'd' |] |] in
  let highlight_mode = 1 in

  (* use an int for highlight_mode *)

  (* game state *)
  let state =
    {
      grid;
      found_words = BatSet.empty;
      (* no words found yet *) guessed_words =
        BatSet.of_list [ "matthew"; "amy" ];
      (* guessed words *) theme = "names";
    }
  in

  (* Word positions and accepted words *)
  let word_positions =
    [ ("matthew", [ (0, 0); (0, 1) ]); ("amy", [ (1, 0); (1, 1) ]) ]
  in
  let target_words = [ "matthew"; "amy" ] in
  let accepted_words = BatSet.of_list [ "matthew"; "amy"; "angie" ] in

  (* convert list to BatSet *)

  (* Less than 3 valid guesses, hint not unlocked *)
  let _ =
    hint_revealer state word_positions target_words accepted_words grid_box
      highlight_mode None
  in

  (* Run GTK main loop *) ()

let test_process_input_word_already_guessed _ =
  let initial_state =
    {
      grid = [| [| 'a'; 'b' |]; [| 'c'; 'd' |] |];
      (* Sample grid *) found_words = BatSet.empty;
      guessed_words = BatSet.of_list [ "matthew" ];
      (* Already guessed "matthew" *) theme = "names";
    }
  in
  let match_counter = ref 0 in
  let hint_counter = ref 0 in
  let max_hints = 3 in
  let accepted_words = BatSet.of_list [ "matthew"; "amy" ] in
  let word_positions =
    [ ("matthew", [ (0, 0); (0, 1) ]); ("amy", [ (1, 0); (1, 1) ]) ]
  in

  let result =
    process_input initial_state "matthew" [ "matthew"; "amy" ] match_counter
      hint_counter max_hints accepted_words word_positions None
  in
  assert_equal initial_state result

let test_process_input_word_found_in_target _ =
  let initial_state =
    {
      grid = [| [| 'a'; 'b' |]; [| 'c'; 'd' |] |];
      found_words = BatSet.empty;
      guessed_words = BatSet.empty;
      (* No words guessed yet *) theme = "names";
    }
  in
  let match_counter = ref 0 in
  let hint_counter = ref 0 in
  let max_hints = 3 in
  let accepted_words = BatSet.of_list [ "matthew"; "amy" ] in
  let word_positions =
    [ ("matthew", [ (0, 0); (0, 1) ]); ("amy", [ (1, 0); (1, 1) ]) ]
  in

  let result =
    process_input initial_state "matthew" [ "matthew"; "amy" ] match_counter
      hint_counter max_hints accepted_words word_positions None
  in
  (* Word to be added to found_words, and the match_counter to increment *)
  let expected_state =
    {
      grid = initial_state.grid;
      found_words = BatSet.add "matthew" initial_state.found_words;
      guessed_words = BatSet.add "matthew" initial_state.guessed_words;
      theme = initial_state.theme;
    }
  in
  assert_equal expected_state result

let test_process_input_valid_word_not_in_target _ =
  let initial_state =
    {
      grid = [| [| 'a'; 'b' |]; [| 'c'; 'd' |] |];
      found_words = BatSet.empty;
      guessed_words = BatSet.empty;
      theme = "names";
    }
  in
  let match_counter = ref 0 in
  let hint_counter = ref 0 in
  let max_hints = 3 in
  let accepted_words = BatSet.of_list [ "matthew"; "amy" ] in
  let word_positions =
    [ ("matthew", [ (0, 0); (0, 1) ]); ("amy", [ (1, 0); (1, 1) ]) ]
  in

  let result =
    process_input initial_state "bojak" [ "matthew"; "amy" ] match_counter
      hint_counter max_hints accepted_words word_positions None
  in
  (* "bojak" to be counted towards hint_counter *)
  let expected_state =
    {
      grid = initial_state.grid;
      found_words = initial_state.found_words;
      guessed_words = BatSet.add "bojak" initial_state.guessed_words;
      theme = initial_state.theme;
    }
  in
  assert_equal expected_state result
(* Expect the message: "Word count towards hint incremented. Total words
   guessed: 1" *)

let test_process_input_invalid_word _ =
  let initial_state =
    {
      grid = [| [| 'a'; 'b' |]; [| 'c'; 'd' |] |];
      found_words = BatSet.empty;
      guessed_words = BatSet.empty;
      theme = "names";
    }
  in
  let match_counter = ref 0 in
  let hint_counter = ref 0 in
  let max_hints = 3 in
  let accepted_words = BatSet.of_list [ "matthew"; "amy" ] in
  let word_positions =
    [ ("matthew", [ (0, 0); (0, 1) ]); ("amy", [ (1, 0); (1, 1) ]) ]
  in

  let result =
    process_input initial_state "batman" [ "matthew"; "amy" ] match_counter
      hint_counter max_hints accepted_words word_positions None
  in
  (* "batman" rejected as invalid *)
  let expected_state =
    {
      grid = initial_state.grid;
      found_words = initial_state.found_words;
      guessed_words = BatSet.add "batman" initial_state.guessed_words;
      theme = initial_state.theme;
    }
  in
  assert_equal expected_state result

let test_process_input_no_match _ =
  let initial_state =
    {
      grid = [| [| 'a'; 'b' |]; [| 'c'; 'd' |] |];
      found_words = BatSet.empty;
      guessed_words = BatSet.empty;
      theme = "names";
    }
  in
  let match_counter = ref 0 in
  let hint_counter = ref 0 in
  let max_hints = 3 in
  let accepted_words = BatSet.of_list [ "matthew"; "amy" ] in
  let word_positions =
    [ ("matthew", [ (0, 0); (0, 1) ]); ("amy", [ (1, 0); (1, 1) ]) ]
  in

  let result =
    process_input initial_state "falak" [ "matthew"; "amy" ] match_counter
      hint_counter max_hints accepted_words word_positions None
  in
  (* "falak" to be added to guessed_words without any changes to found_words or
     hint_counter *)
  let expected_state =
    {
      grid = initial_state.grid;
      found_words = initial_state.found_words;
      guessed_words = BatSet.add "falak" initial_state.guessed_words;
      theme = initial_state.theme;
    }
  in
  assert_equal expected_state result

(* "Invalid word: falak" *)
let test_is_spangram _ =
  (*The word matches the spangram *)
  let result = is_spangram "matthew" "matthew" in
  assert_equal true result;

  (* The word does not match the spangram *)
  let result = is_spangram "matthew" "amy" in
  assert_equal false result;

  (* Test Case 3: The word is different, but spangram is empty *)
  let result = is_spangram "matthew" "" in
  assert_equal false result;

  (* The word is the same, but spangram is empty *)
  let result = is_spangram "" "" in
  assert_equal true result;

  (* Case-sensitive comparison (word is "Matthew", spangram is "matthew") *)
  let result = is_spangram "Matthew" "matthew" in
  assert_equal false result (* Expected: false *)

let suite =
  "TestGame"
  >::: [
         "test_make_state" >:: test_make_state;
         "test_make_state2" >:: test_make_state2;
         "test_print_letter" >:: test_print_letter;
         "test_is_highlighted" >:: test_is_highlighted;
         "test_handle_guess" >:: test_handle_guess;
         "test_is_word_in_grid" >:: test_is_word_in_grid;
         "test_alr_guessed" >:: test_alr_guessed;
         "test_get_letter" >:: test_get_letter;
         "test_print_letter_yellow" >:: test_print_letter_yellow;
         "test_print_letter_green" >:: test_print_letter_green;
         "test_print_letter_blue" >:: test_print_letter_blue;
         "test_find_index" >:: test_find_index;
         "test_word_to_highlight" >:: test_word_to_highlight;
         "test_load_words" >:: test_load_words;
         "test_load_words" >:: test_load_words;
         "test_word_to_highlight" >:: test_word_to_highlight;
         "test_hint_revealer" >:: test_hint_revealer;
         "test_process_input_word_already_guessed"
         >:: test_process_input_word_already_guessed;
         "test_process_input_word_found_in_target"
         >:: test_process_input_word_found_in_target;
         "test_process_input_valid_word_not_in_target"
         >:: test_process_input_valid_word_not_in_target;
         "test_process_input_invalid_word" >:: test_process_input_invalid_word;
         "test_process_input_no_match" >:: test_process_input_no_match;
         "test_is_spangram" >:: test_is_spangram;
       ]

let () = run_test_tt_main suite
