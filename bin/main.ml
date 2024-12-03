open GMain
open GPango
open Printf
open Cs3110_fin.Logic

(* Initialize GTK *)
let () = ignore (GMain.init ())

(* Create a new window *)
let window = GWindow.window ~title:"OCaml Strands" ~border_width:10 ()

type letter = char
type grid = letter array array

let initial_grid : grid =
  [|
    [| 'S'; 'N'; 'P'; 'M'; 'D'; 'S' |];
    [| 'I'; 'K'; 'U'; 'I'; 'R'; 'E' |];
    [| 'E'; 'A'; 'M'; 'P'; 'H'; 'Y' |];
    [| 'Z'; 'R'; 'H'; 'A'; 'D'; 'A' |];
    [| 'O'; 'U'; 'C'; 'G'; 'R'; 'S' |];
    [| 'T'; 'C'; 'N'; 'H'; 'U'; 'P' |];
    [| 'R'; 'S'; 'I'; 'O'; 'P'; 'L' |];
    [| 'E'; 'D'; 'D'; 'A'; 'E'; 'S' |];
  |]

let target_words =
  [ "hayrides"; "pumpkins"; "maze"; "cider"; "doughnuts"; "apples"; "orchards" ]

let word_positions =
  [
    ( "hayrides",
      [ (2, 4); (3, 5); (2, 5); (1, 4); (1, 3); (0, 4); (1, 5); (0, 5) ] );
    ( "pumpkins",
      [ (2, 3); (1, 2); (0, 3); (0, 2); (1, 1); (1, 0); (0, 1); (0, 0) ] );
    ("maze", [ (2, 2); (2, 1); (3, 0); (2, 0) ]);
    ("cider", [ (5, 1); (6, 2); (7, 1); (7, 0); (6, 0) ]);
    ( "doughnuts",
      [ (7, 2); (6, 3); (5, 4); (4, 3); (5, 3); (5, 2); (4, 1); (5, 0); (6, 1) ]
    );
    ("apples", [ (7, 3); (6, 4); (5, 5); (6, 5); (7, 4); (7, 5) ]);
    ( "orchards",
      [ (4, 0); (3, 1); (4, 2); (3, 2); (3, 3); (4, 4); (3, 4); (4, 5) ] );
  ]

type game_state = {
  grid : grid;
  found_words : string list;
}

let print_letter letter highlight =
  if highlight then Printf.printf "\027[1;32m%c\027[0m " letter
  else Printf.printf "%c " letter

let is_highlighted (r, c) found_words =
  List.exists
    (fun (word, positions) ->
      List.mem word found_words && List.mem (r, c) positions)
    word_positions

let print_grid (grid : letter array array) found_words =
  Array.iteri
    (fun r row ->
      Array.iteri
        (fun c letter ->
          print_letter letter (is_highlighted (r, c) found_words))
        row;
      print_newline ())
    grid

let handle_guess state guess =
  let lower_guess = String.lowercase_ascii guess in
  if
    List.mem lower_guess target_words
    && not (List.mem lower_guess state.found_words)
  then { state with found_words = lower_guess :: state.found_words }
  else state

let rec game_loop state match_counter hint_counter max_hints accepted_words =
  Printf.printf "Guess a word: ";
  let guess = read_line () in
  let new_state =
    Cs3110_fin.Logic.process_input state guess target_words match_counter
      hint_counter max_hints accepted_words
  in
  Cs3110_fin.Logic.print_grid new_state.grid new_state.found_words
    word_positions;
  if List.length new_state.found_words = List.length target_words then (
    Printf.printf "YAY congrats! You found all the words (:\n";
    exit 0)
  else game_loop new_state match_counter hint_counter max_hints accepted_words

(* Execute *)
let () =
  let accepted_words =
    Cs3110_fin.Logic.load_words "data/filtered_accepted_words.csv"
  in

  (* Debugging check for "abrupt" *)
  if BatSet.mem "abrupt" accepted_words then
    Printf.printf "Word abrupt is in the set\n"
  else Printf.printf "Word abrupt is NOT in the set\n";

  (* Initialize other game components *)
  let initial_state =
    { Cs3110_fin.Logic.grid = initial_grid; found_words = [] }
  in
  let match_counter = ref 0 in
  let hint_counter = ref 0 in
  let max_hints = 3 in

  ignore (window#connect#destroy ~callback:Main.quit);
  let vbox = GPack.vbox ~border_width:20 ~packing:window#add () in
  let title_label = GMisc.label ~text:"OCaml Strands" ~packing:vbox#pack () in
  title_label#misc#modify_font (GPango.font_description_from_string "Serif 20");
  let start_button = GButton.button ~label:"Play" ~packing:vbox#pack () in
  ignore
    (start_button#connect#clicked ~callback:(fun () ->
         print_endline "Theme: Fall Fun";
         Cs3110_fin.Logic.print_grid initial_state.grid
           initial_state.found_words word_positions;
         game_loop initial_state match_counter hint_counter max_hints
           accepted_words));
  window#show ();
  Main.main ()
